{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import Data.Time (ZonedTime)
import GHC.Clock qualified as Clock
import NOM.Builds (Derivation (..), FailType (..), Host (..), HostContext (WithContext), StorePath (..), storePrefix)
-- INVENTORY: package.1.2
import NOM.CacheDiscovery (newResolver)
import NOM.CacheDiscovery qualified as CacheDiscovery
import NOM.CacheDiscovery.SourceMap (parseSourceMap, proxyCandidates, rewriteState)
import NOM.CacheDiscovery.SourceMap qualified as SourceMap
import NOM.Error (NOMError)
import NOM.IO (Stream, interact, processTextStream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..))
import NOM.IO.Input.JSON ()
import NOM.NixMessage.JSON (ActivityId (..), NixJSONMessage)
import NOM.Print (Config (..))
import NOM.State (BuildFail (..), BuildInfo (..), BuildStatus (..), DependencySummary (..), DerivationId, DerivationInfo (..), NOMState (..), StorePathId, StorePathInfo (..), TransferInfo (..))
import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Initial (initialStateFromBuildPlatform)
import NOM.Update (maintainState)
import NOM.Update.Monad (UpdateMonad)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Relude
import Streamly.Data.Stream qualified as Stream
import System.Console.Terminal.Size (Window)
import System.Directory (doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath (takeExtension)
import System.IO qualified as SystemIO
import System.IO.Unsafe qualified as UnsafeIO
import System.Posix.IO qualified as Posix
-- INVENTORY: package.4.4
import Test.HUnit (
  Counts (errors, failures),
  Test,
  Testable (test),
  assertBool,
  assertEqual,
  assertFailure,
  runTestTT,
  (~:),
 )

hashPart :: Text
hashPart = "0123456789abcdef0123456789abcdef"

source :: Text
source = "https://cache.nixos.org"

firstHash :: Text
firstHash = Text.replicate 32 "a"

secondHash :: Text
secondHash = Text.replicate 32 "b"

proxyInfo :: ByteString
proxyInfo = "version=1\nid=abc\n"

infoPath :: Text
infoPath = "/.nix-cache-discovery/v1/info"

sourcesPath :: Text
sourcesPath = "/.nix-cache-discovery/v1/sources"

sourceLine :: Text -> Text -> ByteString
sourceLine hashText url = encodeUtf8 (hashText <> " " <> url <> "\n")

sourceHost :: Text -> Host WithContext
sourceHost = Host (Just "https") Nothing

baseState :: IO NOMState
baseState = initialStateFromBuildPlatform (Just "x86_64-linux")

addRunning :: Host WithContext -> Text -> NOMState -> NOMState
addRunning host hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      path = StorePath hashText "example"
      transferInfo =
        MkTransferInfo
          { host = host
          , start = 0
          , activityId = Strict.Nothing
          , end = ()
          }
      summary = state.fullSummary
      newSummary = summary{runningDownloads = CMap.insert pathId transferInfo summary.runningDownloads}
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo path mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = newSummary
        }

addCompleted :: Host WithContext -> Text -> NOMState -> NOMState
addCompleted host hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      path = StorePath hashText "example"
      transferInfo =
        MkTransferInfo
          { host = host
          , start = 0
          , activityId = Strict.Nothing
          , end = Strict.Just 1.0
          }
      summary = state.fullSummary
      newSummary = summary{completedDownloads = CMap.insert pathId transferInfo summary.completedDownloads}
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo path mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = newSummary
        }

stateWithDownloads :: [(Host WithContext, Text)] -> IO NOMState
stateWithDownloads downloads = do
  base <- baseState
  pure (foldl' (\state (host, hashText) -> addRunning host hashText state) base downloads)

loopbackHost :: Int -> Host WithContext
loopbackHost port = Host Nothing Nothing ("127.0.0.1:" <> Text.pack (show port))

hostOf :: StorePathId -> NOMState -> Maybe (Host WithContext)
hostOf pathId state = fmap (.host) (CMap.lookup pathId state.fullSummary.runningDownloads)

awaitHosts :: (NOMState -> IO NOMState) -> NOMState -> [(StorePathId, Host WithContext)] -> IO NOMState
awaitHosts resolve state expectations = go (0 :: Int)
 where
  go attempt = do
    resolved <- resolve state
    let satisfied = all (\(pathId, expected) -> hostOf pathId resolved == Just expected) expectations
    if satisfied || attempt >= 100
      then pure resolved
      else do
        Concurrent.threadDelay 20_000
        go (attempt + 1)

waitForRequestCount :: Stub -> Int -> IO ()
waitForRequestCount stub expected = go (0 :: Int)
 where
  go attempt = do
    requests <- stub.stubLog
    when (length requests < expected && attempt < 200) do
      Concurrent.threadDelay 10_000
      go (attempt + 1)

infoRequests :: Stub -> IO [(Double, Text)]
infoRequests stub = filter (\(_, target) -> target == infoPath) <$> stub.stubLog

sourcesRequests :: Stub -> IO [(Double, Text)]
sourcesRequests stub = filter (\(_, target) -> target == sourcesPath) <$> stub.stubLog

requestTargets :: Stub -> IO [Text]
requestTargets stub = map snd <$> stub.stubLog

repeatResolve :: Int -> (NOMState -> IO NOMState) -> NOMState -> IO ()
repeatResolve repetitions resolve state = when (repetitions > 0) do
  void (resolve state)
  Concurrent.threadDelay 50_000
  repeatResolve (repetitions - 1) resolve state

withProxyEnvironment :: IO a -> IO a
withProxyEnvironment = Exception.bracket_ setProxies unsetProxies
 where
  proxyVariables :: [String]
  proxyVariables = ["http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY"]
  proxyValue :: String
  proxyValue = "http://127.0.0.1:1"
  setProxies = forM_ proxyVariables \name -> setEnv name proxyValue
  unsetProxies = forM_ proxyVariables \name -> unsetEnv name

closedPort :: IO Int
closedPort = do
  listeningSocket <- Socket.socket Socket.AF_INET Socket.Stream 0
  Socket.bind listeningSocket (Socket.SockAddrInet 0 0)
  address <- Socket.getSocketName listeningSocket
  Socket.close listeningSocket
  case address of
    Socket.SockAddrInet socketPort _ -> pure (fromIntegral socketPort)
    _ -> fail "closed port bound to an unexpected address"

readCacheDiscoverySource :: IO Text
readCacheDiscoverySource = do
  candidates <- filterM doesFileExist ["lib/NOM/CacheDiscovery.hs", "nix-output-monitor/lib/NOM/CacheDiscovery.hs"]
  case candidates of
    path : _ -> decodeUtf8 <$> ByteString.readFile path
    [] -> fail "NOM/CacheDiscovery.hs is missing from the test working directory"

readRepoText :: [FilePath] -> IO Text
readRepoText candidates = do
  existing <- filterM doesFileExist candidates
  case existing of
    path : _ -> decodeUtf8 <$> ByteString.readFile path
    [] -> fail ("missing file: " <> show candidates)

readRepoDirectory :: [FilePath] -> IO FilePath
readRepoDirectory candidates = do
  existing <- filterM doesDirectoryExist candidates
  case existing of
    path : _ -> pure path
    [] -> fail ("missing directory: " <> show candidates)

moduleNamesUnder :: FilePath -> FilePath -> IO [Text]
moduleNamesUnder base directory = do
  entries <- listDirectory directory
  concat <$> forM entries \entry -> do
    let fullPath = directory <> "/" <> entry
    isDirectory <- doesDirectoryExist fullPath
    if isDirectory
      then moduleNamesUnder base fullPath
      else if takeExtension entry == ".hs" then pure [moduleName base fullPath] else pure []

moduleName :: FilePath -> FilePath -> Text
moduleName base path =
  let relative = drop (length base + 1) path
      withoutExtension = take (length relative - 3) relative
   in Text.replace "/" "." (Text.pack withoutExtension)

cabalStanzaLines :: Text -> Text -> [Text]
cabalStanzaLines contents header =
  case dropWhile (not . Text.isPrefixOf header) (Text.lines contents) of
    [] -> []
    firstLine : rest -> firstLine : takeWhile (not . isCabalStanzaHeader) rest

isCabalStanzaHeader :: Text -> Bool
isCabalStanzaHeader line =
  not (Text.null line) && not (Text.isPrefixOf " " line) && not (Text.isPrefixOf "\t" line)

cabalField :: Text -> Text -> Text -> [Text]
cabalField contents stanzaHeader field =
  let stanza = map Text.stripStart (cabalStanzaLines contents stanzaHeader)
   in case dropWhile (not . Text.isPrefixOf field) stanza of
        [] -> []
        firstLine : rest ->
          let firstValue = Text.stripStart (Text.drop (Text.length field) firstLine)
           in mapMaybe dependencyName (firstValue : takeWhile (not . Text.isInfixOf ":") rest)

dependencyName :: Text -> Maybe Text
dependencyName line =
  let name = Text.takeWhile (\character -> isAlphaNum character || character == '-') line
   in if Text.null name then Nothing else Just name

exposedModules :: Text -> [Text]
exposedModules contents =
  let block = drop 1 (dropWhile (not . Text.isPrefixOf "exposed-modules:" . Text.stripStart) (Text.lines contents))
   in map Text.strip (takeWhile (not . Text.null . Text.strip) block)

suiteSucceeds :: Counts -> Bool
suiteSucceeds counts = errors counts + failures counts == 0

captureStderr :: IO () -> IO ByteString
captureStderr action = do
  temporaryDirectory <- getTemporaryDirectory
  let path = temporaryDirectory <> "/nom-cache-discovery-stderr.log"
  savedDescriptor <- Posix.dup Posix.stdError
  targetDescriptor <- Posix.createFile path 0o600
  void (Posix.dupTo targetDescriptor Posix.stdError)
  Posix.closeFd targetDescriptor
  captured <-
    (action >> ByteString.readFile path)
      `Exception.finally` void (Posix.dupTo savedDescriptor Posix.stdError)
  Posix.closeFd savedDescriptor
  pure captured

data StubBodies = MkStubBodies
  { infoBody :: ByteString
  , infoStatus :: Int
  , sourcesBody :: ByteString
  , sourcesStatus :: Int
  , responseDelay :: Int
  }

data Stub = MkStub
  { stubPort :: Int
  , stubLog :: IO [(Double, Text)]
  , stubStop :: IO ()
  , stubSetInfoBody :: ByteString -> IO ()
  , stubSetInfoStatus :: Int -> IO ()
  , stubSetSourcesBody :: ByteString -> IO ()
  , stubSetSourcesStatus :: Int -> IO ()
  }

startStub :: Int -> IO Stub
startStub initialDelay = do
  startTime <- Clock.getMonotonicTime
  logVar <- STM.newTVarIO []
  bodiesVar <-
    STM.newTVarIO
      MkStubBodies
        { infoBody = ""
        , infoStatus = 200
        , sourcesBody = ""
        , sourcesStatus = 200
        , responseDelay = initialDelay
        }
  listeningSocket <- Socket.socket Socket.AF_INET Socket.Stream 0
  Socket.bind listeningSocket (Socket.SockAddrInet 0 0)
  Socket.listen listeningSocket 64
  address <- Socket.getSocketName listeningSocket
  port <- case address of
    Socket.SockAddrInet socketPort _ -> pure (fromIntegral socketPort)
    _ -> fail "stub bound to an unexpected address"
  void (Concurrent.forkIO (acceptLoop listeningSocket logVar bodiesVar startTime))
  pure
    MkStub
      { stubPort = port
      , stubLog = STM.readTVarIO logVar
      , stubStop = Socket.close listeningSocket
      , stubSetInfoBody = \body -> STM.atomically (STM.modifyTVar' bodiesVar \old -> old{infoBody = body})
      , stubSetInfoStatus = \status -> STM.atomically (STM.modifyTVar' bodiesVar \old -> old{infoStatus = status})
      , stubSetSourcesBody = \body -> STM.atomically (STM.modifyTVar' bodiesVar \old -> old{sourcesBody = body})
      , stubSetSourcesStatus = \status -> STM.atomically (STM.modifyTVar' bodiesVar \old -> old{sourcesStatus = status})
      }

withStub :: Int -> (Stub -> IO ()) -> IO ()
withStub initialDelay action = do
  stub <- startStub initialDelay
  action stub `Exception.finally` stub.stubStop

acceptLoop :: Socket.Socket -> STM.TVar [(Double, Text)] -> STM.TVar StubBodies -> Double -> IO ()
acceptLoop listeningSocket logVar bodiesVar startTime = do
  outcome <- Exception.try @Exception.SomeException do
    (connection, _) <- Socket.accept listeningSocket
    void (Concurrent.forkIO (serveConnection connection logVar bodiesVar startTime))
  case outcome of
    Left _ -> pure ()
    Right () -> acceptLoop listeningSocket logVar bodiesVar startTime

serveConnection :: Socket.Socket -> STM.TVar [(Double, Text)] -> STM.TVar StubBodies -> Double -> IO ()
serveConnection connection logVar bodiesVar startTime =
  Exception.handle @Exception.SomeException (\_ -> pure ()) do
    requestBytes <- readRequest connection mempty
    now <- Clock.getMonotonicTime
    let requestLine = fromMaybe "" (listToMaybe (Text.lines (decodeUtf8 requestBytes)))
        target = fromMaybe "" (listToMaybe (drop 1 (Text.words requestLine)))
    STM.atomically (STM.modifyTVar' logVar (<> [(now - startTime, target)]))
    bodies <- STM.readTVarIO bodiesVar
    Concurrent.threadDelay bodies.responseDelay
    let (status, responseBody) = responseFor target bodies
    SocketBytes.sendAll connection (renderResponse status responseBody)
    Socket.close connection

readRequest :: Socket.Socket -> ByteString -> IO ByteString
readRequest connection accumulated
  | "\r\n\r\n" `ByteString.isInfixOf` accumulated = pure accumulated
  | ByteString.length accumulated > 65536 = pure accumulated
  | otherwise = do
      chunk <- SocketBytes.recv connection 4096
      if ByteString.null chunk
        then pure accumulated
        else readRequest connection (accumulated <> chunk)

responseFor :: Text -> StubBodies -> (Int, ByteString)
responseFor target bodies
  | target == infoPath = answer bodies.infoStatus bodies.infoBody
  | target == sourcesPath = answer bodies.sourcesStatus bodies.sourcesBody
  | otherwise = (404, "")
 where
  answer status body =
    if status == 200
      then (status, body)
      else (status, "")

renderResponse :: Int -> ByteString -> ByteString
renderResponse status body =
  "HTTP/1.1 "
    <> encodeUtf8 (Text.pack (show status))
    <> " "
    <> encodeUtf8 (reasonPhrase status)
    <> "\r\nContent-Length: "
    <> encodeUtf8 (Text.pack (show (ByteString.length body)))
    <> "\r\nConnection: close\r\n\r\n"
    <> body

reasonPhrase :: Int -> Text
reasonPhrase status = case status of
  200 -> "OK"
  404 -> "Not Found"
  500 -> "Internal Server Error"
  _ -> "Unknown"

testState :: IO NOMState
testState = do
  initial <- initialStateFromBuildPlatform (Just "x86_64-linux")
  let path = StorePath hashPart "example"
      pathId = MkCacheId 0
      transferInfo =
        MkTransferInfo
          { host = Host (Just "http") Nothing "127.0.0.1:5055"
          , start = 0
          , activityId = Strict.Nothing
          , end = ()
          }
      summary = (mempty :: DependencySummary){runningDownloads = CMap.insert pathId transferInfo mempty}
  pure
    initial
      { storePathInfos = CMap.insert pathId (MkStorePathInfo path mempty Strict.Nothing mempty) mempty
      , fullSummary = summary
      }

addRunningTransfer :: Text -> TransferInfo () -> NOMState -> NOMState
addRunningTransfer hashText transferInfo state =
  let pathId = CMap.nextKey state.storePathInfos
      path = StorePath hashText "example"
      summary = state.fullSummary
      newSummary = summary{runningDownloads = CMap.insert pathId transferInfo summary.runningDownloads}
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo path mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = newSummary
        }

addRunningUpload :: Host WithContext -> Text -> NOMState -> NOMState
addRunningUpload host hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      transferInfo = MkTransferInfo host 0 Strict.Nothing ()
      summary = state.fullSummary
      newSummary = summary{runningUploads = CMap.insert pathId transferInfo summary.runningUploads}
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo (StorePath hashText "example") mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = newSummary
        }

addCompletedUpload :: Host WithContext -> Text -> NOMState -> NOMState
addCompletedUpload host hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      transferInfo = MkTransferInfo host 0 Strict.Nothing (Strict.Just 1.0)
      summary = state.fullSummary
      newSummary = summary{completedUploads = CMap.insert pathId transferInfo summary.completedUploads}
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo (StorePath hashText "example") mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = newSummary
        }

addPlannedDownload :: Text -> NOMState -> NOMState
addPlannedDownload hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      summary = state.fullSummary
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo (StorePath hashText "example") mempty Strict.Nothing mempty) state.storePathInfos
        , fullSummary = summary{plannedDownloads = CSet.insert pathId summary.plannedDownloads}
        }

addDerivationDownload :: DerivationId -> Host WithContext -> Text -> NOMState -> NOMState
addDerivationDownload derivationId host hashText state =
  let pathId = CMap.nextKey state.storePathInfos
      path = StorePath hashText "example"
      transferInfo = MkTransferInfo host 0 Strict.Nothing ()
      summary = (mempty :: DependencySummary){runningDownloads = CMap.insert pathId transferInfo mempty}
      derivationInfo = MkDerivationInfo (Derivation path) mempty mempty mempty Unknown summary False mempty Strict.Nothing Strict.Nothing
   in state
        { storePathInfos = CMap.insert pathId (MkStorePathInfo path mempty Strict.Nothing mempty) state.storePathInfos
        , derivationInfos = CMap.insert derivationId derivationInfo state.derivationInfos
        }

derivationDownloadHost :: DerivationId -> StorePathId -> NOMState -> Maybe (Host WithContext)
derivationDownloadHost derivationId pathId state =
  CMap.lookup derivationId state.derivationInfos
    >>= ( \derivationInfo ->
            CMap.lookup pathId derivationInfo.dependencySummary.runningDownloads
        )
    >>= (Just . (.host))

addBuildsAndPlan :: NOMState -> NOMState
addBuildsAndPlan state =
  let summary = state.fullSummary
      completedInfo = MkBuildInfo 0 Localhost Strict.Nothing Strict.Nothing 1.0
   in state
        { fullSummary =
            summary
              { completedBuilds = CMap.insert (MkCacheId 5) completedInfo summary.completedBuilds
              , plannedBuilds = CSet.fromFoldable [MkCacheId 6]
              }
        }

addBuildAndFailure :: NOMState -> NOMState
addBuildAndFailure state =
  let summary = state.fullSummary
      completedInfo = MkBuildInfo 0 Localhost Strict.Nothing Strict.Nothing 1.0
      failedInfo = MkBuildInfo 0 Localhost Strict.Nothing Strict.Nothing (MkBuildFail 1.0 HashMismatch)
   in state
        { fullSummary =
            summary
              { completedBuilds = CMap.insert (MkCacheId 7) completedInfo summary.completedBuilds
              , failedBuilds = CMap.insert (MkCacheId 8) failedInfo summary.failedBuilds
              }
        }

testConfig :: Config
testConfig = MkConfig False False

jsonLine :: Text -> ByteString
jsonLine message = encodeUtf8 ("@nix {\"action\":\"msg\",\"level\":3,\"msg\":\"" <> message <> "\"}")

copyPathLine :: Text -> ByteString
copyPathLine hostText =
  encodeUtf8
    ( "@nix {\"action\":\"start\",\"id\":1,\"level\":3,\"text\":\"copying path\",\"type\":100,\"fields\":[\""
        <> storePrefix
        <> hashPart
        <> "-example\",\""
        <> hostText
        <> "\",\"local\"]}"
    )

delayedLineStream :: Int -> [ByteString] -> Stream ByteString
delayedLineStream delayMicroseconds inputLines = Stream.mapM delayLine (Stream.fromList inputLines)
 where
  delayLine line = do
    Concurrent.threadDelay delayMicroseconds
    pure line

jsonStateUpdater :: (UpdateMonad m) => NixJSONMessage -> StateT NOMState m ([NOMError], ByteString, Bool)
jsonStateUpdater input = do
  oldState <- get
  updateResult <- updateState @NixJSONMessage input oldState
  put updateResult.newState
  pure (mempty, mempty, False)

jsonFinalizer :: (Monad m) => StateT NOMState m ()
jsonFinalizer = pure ()

runJsonProcess ::
  Maybe (NOMState -> Maybe (Window Int) -> (ZonedTime, Double) -> Text, Handle) ->
  (Double -> NOMState -> NOMState) ->
  (NOMState -> IO NOMState) ->
  NOMState ->
  Stream ByteString ->
  IO NOMState
runJsonProcess printerMay maintenance resolve initialState inputStream =
  withParser @NixJSONMessage \parser ->
    processTextStream
      testConfig
      parser
      jsonStateUpdater
      maintenance
      resolve
      printerMay
      jsonFinalizer
      initialState
      (Right <$> inputStream)

withNullHandle :: (Handle -> IO a) -> IO a
withNullHandle action = do
  temporaryDirectory <- getTemporaryDirectory
  SystemIO.withFile (temporaryDirectory <> "/nom-cache-discovery-io-output.txt") SystemIO.WriteMode action

countingResolve :: IORef Int -> NOMState -> IO NOMState
countingResolve counter state = do
  atomicModifyIORef' counter \count -> (count + 1, ())
  pure state

identityResolve :: NOMState -> IO NOMState
identityResolve = pure

rewriteResolve :: NOMState -> IO NOMState
rewriteResolve state = pure (rewriteState (Map.singleton hashPart source) state)

touchedResolve :: IORef (Maybe Int) -> NOMState -> IO NOMState
touchedResolve recording state = do
  writeIORef recording (Just (CSet.size state.touchedIds))
  pure state

countingPrinter :: IORef Int -> NOMState -> Maybe (Window Int) -> (ZonedTime, Double) -> Text
countingPrinter counter _ _ _ = UnsafeIO.unsafePerformIO do
  atomicModifyIORef' counter \count -> (count + 1, ())
  pure ""
{-# NOINLINE countingPrinter #-}

hostRecordingPrinter :: IORef (Maybe (Host WithContext)) -> NOMState -> Maybe (Window Int) -> (ZonedTime, Double) -> Text
hostRecordingPrinter recording state _ _ = UnsafeIO.unsafePerformIO do
  writeIORef recording (hostOf (MkCacheId 0) state)
  pure ""
{-# NOINLINE hostRecordingPrinter #-}

summaryText :: NOMState -> Text
summaryText state =
  Text.pack
    . show
    $ ( CMap.size state.fullSummary.runningDownloads
      , CMap.size state.fullSummary.completedDownloads
      , CMap.size state.fullSummary.completedBuilds
      , CMap.size state.fullSummary.failedBuilds
      , CSet.size state.fullSummary.plannedBuilds
      )

summaryRecordingPrinter :: IORef (Maybe Text) -> NOMState -> Maybe (Window Int) -> (ZonedTime, Double) -> Text
summaryRecordingPrinter recording state _ _ = UnsafeIO.unsafePerformIO do
  writeIORef recording (Just (summaryText state))
  pure ""
{-# NOINLINE summaryRecordingPrinter #-}

touchedInitialState :: IO NOMState
touchedInitialState = do
  base <- baseState
  let derivation = Derivation (StorePath hashPart "example")
      derivationInfo = MkDerivationInfo derivation mempty mempty mempty Unknown mempty False mempty Strict.Nothing Strict.Nothing
  pure
    base
      { derivationInfos = CMap.insert (MkCacheId 1) derivationInfo base.derivationInfos
      , touchedIds = CSet.fromFoldable [MkCacheId 1]
      }

readIntegrationReplay :: Text -> IO ByteString
readIntegrationReplay name = do
  let candidates =
        [ "test/integration/" <> name <> "/stderr.json"
        , "nix-output-monitor/test/integration/" <> name <> "/stderr.json"
        ]
  existingFiles <- filterM doesFileExist (toString <$> candidates)
  case existingFiles of
    path : _ -> ByteString.readFile path
    [] -> fail "the integration replay is missing from the working directory"

-- INVENTORY: test.1.4
tests :: IO [Test]
tests = do
  nomState <- testState
  pure
    [ -- INVENTORY: sourcemap.1.1
      "parse source map" ~: do
        let input = encodeUtf8 (hashPart <> " " <> source <> "\nnot a source line\n")
        assertEqual "source map is parsed" (Map.singleton hashPart source) (parseSourceMap input)
    , -- INVENTORY: protocol.4.1
      -- INVENTORY: sourcemap.2.1
      "detect proxy candidates" ~: do
        assertEqual
          "loopback download host is a proxy candidate"
          (Set.singleton "http://127.0.0.1:5055")
          (proxyCandidates nomState)
    , -- INVENTORY: sourcemap.3.1
      -- INVENTORY: sourcemap.5.1
      -- INVENTORY: sourcemap.6.1
      "rewrite download host" ~: do
        let rewritten = rewriteState (Map.singleton hashPart source) nomState
        assertEqual
          "source host replaces the proxy host"
          (Just (Host (Just "https") Nothing "cache.nixos.org"))
          (fmap (.host) (CMap.lookup (MkCacheId 0) rewritten.fullSummary.runningDownloads))
    , -- INVENTORY: sourcemap.1.2
      "a 31-character hash line is skipped" ~: do
        let input = sourceLine (Text.replicate 31 "a") source
        assertEqual "no entries" Map.empty (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.3
      "a 33-character hash line is skipped" ~: do
        let input = sourceLine (Text.replicate 33 "a") source
        assertEqual "no entries" Map.empty (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.4
      "a line with three words is skipped" ~: do
        let input = encodeUtf8 (firstHash <> " " <> source <> " extra\n")
        assertEqual "no entries" Map.empty (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.5
      "a line with one word is skipped" ~: do
        let input = encodeUtf8 (firstHash <> "\n")
        assertEqual "no entries" Map.empty (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.6
      "blank and junk lines are skipped around valid lines" ~: do
        let input =
              sourceLine firstHash "https://a.example.org"
                <> "\nnot a source line\n"
                <> sourceLine secondHash "https://b.example.org"
        assertEqual
          "two entries are kept"
          (Map.fromList [(firstHash, "https://a.example.org"), (secondHash, "https://b.example.org")])
          (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.7
      "the last duplicate hash line wins" ~: do
        let input = sourceLine hashPart "https://old.example.org" <> sourceLine hashPart "https://new.example.org"
        assertEqual "the new URL wins" (Map.singleton hashPart "https://new.example.org") (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.8
      "a final line without a newline is parsed" ~: do
        let input = encodeUtf8 (hashPart <> " " <> source)
        assertEqual "the entry is parsed" (Map.singleton hashPart source) (parseSourceMap input)
    , -- INVENTORY: sourcemap.1.9
      "a 32-character non-hex hash is accepted" ~: do
        let nonHexHash = "ghijklmnopqrstuvwxyz0123456789ab"
        assertEqual "the entry is kept" (Map.singleton nonHexHash source) (parseSourceMap (sourceLine nonHexHash source))
    , -- INVENTORY: sourcemap.1.10
      "an empty body yields an empty map" ~: do
        assertEqual "no entries" Map.empty (parseSourceMap "")
    -- INVENTORY: sourcemap.1.11
    , "invalid UTF-8 is decoded leniently" ~: do
        assertEqual "invalid bytes yield no entries" Map.empty (parseSourceMap (ByteString.pack [0xff, 0xfe]))
    , -- INVENTORY: sourcemap.2.2
      "a completed download is a proxy candidate" ~: do
        base <- baseState
        let state = addCompleted (Host (Just "http") Nothing "127.0.0.1:5055") hashPart base
        assertEqual "the completed download is a candidate" (Set.singleton "http://127.0.0.1:5055") (proxyCandidates state)
    , -- INVENTORY: sourcemap.2.3
      "a host in both download maps yields one candidate" ~: do
        base <- baseState
        let host = Host (Just "http") Nothing "127.0.0.1:5055"
            pathId = MkCacheId 0
            runningTransfer = MkTransferInfo host 0 Strict.Nothing ()
            completedTransfer = MkTransferInfo host 0 Strict.Nothing (Strict.Just 1.0)
            summary =
              (mempty :: DependencySummary)
                { runningDownloads = CMap.insert pathId runningTransfer mempty
                , completedDownloads = CMap.insert pathId completedTransfer mempty
                }
            state =
              base
                { storePathInfos = CMap.insert pathId (MkStorePathInfo (StorePath hashPart "example") mempty Strict.Nothing mempty) mempty
                , fullSummary = summary
                }
        assertEqual "one candidate" (Set.singleton "http://127.0.0.1:5055") (proxyCandidates state)
    , -- INVENTORY: sourcemap.2.4
      "a non-loopback download yields no candidate" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "10.0.0.1") hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- INVENTORY: sourcemap.2.5
      "a state with no downloads yields no candidate" ~: do
        base <- baseState
        assertEqual "no candidate" Set.empty (proxyCandidates base)
    , -- INVENTORY: sourcemap.2.6
      "a running upload yields no candidate" ~: do
        base <- baseState
        let state = addRunningUpload (loopbackHost 5055) hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- INVENTORY: sourcemap.3.2
      "a completed download in the full summary is rewritten" ~: do
        base <- baseState
        let rewritten = rewriteState (Map.singleton hashPart source) (addCompleted (loopbackHost 5055) hashPart base)
        assertEqual
          "the completed download is rewritten"
          (Just (sourceHost "cache.nixos.org"))
          (fmap (.host) (CMap.lookup (MkCacheId 0) rewritten.fullSummary.completedDownloads))
    , -- INVENTORY: sourcemap.3.3
      "a download inside a derivation summary is rewritten" ~: do
        base <- baseState
        let rewritten =
              rewriteState
                (Map.singleton hashPart source)
                (addDerivationDownload (MkCacheId 1) (loopbackHost 5055) hashPart base)
        assertEqual
          "the derivation download is rewritten"
          (Just (sourceHost "cache.nixos.org"))
          (derivationDownloadHost (MkCacheId 1) (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.3.4
      "every derivation summary is rewritten" ~: do
        base <- baseState
        let state =
              addDerivationDownload
                (MkCacheId 2)
                (loopbackHost 5056)
                secondHash
                (addDerivationDownload (MkCacheId 1) (loopbackHost 5055) hashPart base)
            rewritten = rewriteState (Map.fromList [(hashPart, source), (secondHash, source)]) state
        assertEqual
          "the first derivation download is rewritten"
          (Just (sourceHost "cache.nixos.org"))
          (derivationDownloadHost (MkCacheId 1) (MkCacheId 0) rewritten)
        assertEqual
          "the second derivation download is rewritten"
          (Just (sourceHost "cache.nixos.org"))
          (derivationDownloadHost (MkCacheId 2) (MkCacheId 1) rewritten)
    , -- INVENTORY: sourcemap.3.5
      "an empty source map returns the state unchanged" ~: do
        base <- baseState
        let state = addRunning (loopbackHost 5055) hashPart base
        assertEqual "the state is unchanged" state (rewriteState Map.empty state)
    , -- INVENTORY: sourcemap.3.6
      "a download with no store path info keeps its host" ~: do
        base <- baseState
        let transferInfo = MkTransferInfo (loopbackHost 5055) 0 Strict.Nothing ()
            summary = (mempty :: DependencySummary){runningDownloads = CMap.insert (MkCacheId 0) transferInfo mempty}
            state = base{fullSummary = summary}
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "the host is kept" (Just (loopbackHost 5055)) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.3.7
      "a rewritten state keeps its builds and planned downloads" ~: do
        base <- baseState
        let state = addRunning (loopbackHost 5055) hashPart (addBuildsAndPlan base)
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "completed builds are kept" state.fullSummary.completedBuilds rewritten.fullSummary.completedBuilds
        assertEqual "planned builds are kept" state.fullSummary.plannedBuilds rewritten.fullSummary.plannedBuilds
        assertEqual "the download is rewritten" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.3.8
      "rewriting a rewritten state is stable" ~: do
        base <- baseState
        let state = addRunning (loopbackHost 5055) hashPart base
            sourceMap = Map.singleton hashPart source
            firstRewrite = rewriteState sourceMap state
            secondRewrite = rewriteState sourceMap firstRewrite
        assertEqual "the second rewrite equals the first" firstRewrite secondRewrite
    , -- INVENTORY: sourcemap.4.1
      "a running upload keeps its host" ~: do
        base <- baseState
        let rewritten =
              rewriteState
                (Map.singleton hashPart source)
                (addRunningUpload (loopbackHost 5055) hashPart base)
        assertEqual
          "the upload host is kept"
          (Just (loopbackHost 5055))
          (fmap (.host) (CMap.lookup (MkCacheId 0) rewritten.fullSummary.runningUploads))
    , -- INVENTORY: sourcemap.4.2
      "a completed upload keeps its host" ~: do
        base <- baseState
        let rewritten =
              rewriteState
                (Map.singleton hashPart source)
                (addCompletedUpload (loopbackHost 5055) hashPart base)
        assertEqual
          "the upload host is kept"
          (Just (loopbackHost 5055))
          (fmap (.host) (CMap.lookup (MkCacheId 0) rewritten.fullSummary.completedUploads))
    , -- INVENTORY: sourcemap.4.3
      "planned downloads are unchanged by the rewrite" ~: do
        base <- baseState
        let state = addPlannedDownload hashPart base
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "planned downloads are unchanged" state.fullSummary.plannedDownloads rewritten.fullSummary.plannedDownloads
    , -- INVENTORY: sourcemap.4.4
      "build sets and failed builds are unchanged by the rewrite" ~: do
        base <- baseState
        let state = addBuildAndFailure base
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "completed builds are unchanged" state.fullSummary.completedBuilds rewritten.fullSummary.completedBuilds
        assertEqual "failed builds are unchanged" state.fullSummary.failedBuilds rewritten.fullSummary.failedBuilds
    , -- INVENTORY: sourcemap.5.2
      "a path without a source entry keeps its host" ~: do
        base <- baseState
        let state = addRunning (loopbackHost 5055) hashPart base
            rewritten = rewriteState (Map.singleton secondHash source) state
        assertEqual "the host is kept" (Just (loopbackHost 5055)) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.5.3
      "a non-proxy host is left alone even with a source entry" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "10.0.0.1") hashPart base
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "the public host is kept" (Just (Host Nothing Nothing "10.0.0.1")) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.5.4
      "the lookup uses the 32-character hash part" ~: do
        base <- baseState
        let state = addRunning (loopbackHost 5055) hashPart base
            correct = rewriteState (Map.singleton hashPart source) state
            wrong = rewriteState (Map.singleton "ffffffffffffffffffffffffffffffff" source) state
        assertEqual "the hash part finds the entry" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) correct)
        assertEqual "a wrong key keeps the host" (Just (loopbackHost 5055)) (hostOf (MkCacheId 0) wrong)
    , -- INVENTORY: sourcemap.5.5
      "a source entry without a protocol has no protocol" ~: do
        base <- baseState
        let rewritten =
              rewriteState
                (Map.singleton hashPart "cache.example.org")
                (addRunning (loopbackHost 5055) hashPart base)
        assertEqual "the source has no protocol" (Just (Host Nothing Nothing "cache.example.org")) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.5.6
      "a local source rewrites the host to Localhost" ~: do
        base <- baseState
        let rewritten =
              rewriteState
                (Map.singleton hashPart "local")
                (addRunning (loopbackHost 5055) hashPart base)
        assertEqual "the source is local" (Just Localhost) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.5.7
      "the transfer start and activity id survive the rewrite" ~: do
        base <- baseState
        let transferInfo = MkTransferInfo (loopbackHost 5055) 5 (Strict.Just (MkId 7)) ()
            state = addRunningTransfer hashPart transferInfo base
            rewritten = rewriteState (Map.singleton hashPart source) state
        case CMap.lookup (MkCacheId 0) rewritten.fullSummary.runningDownloads of
          Nothing -> assertFailure "the transfer disappeared"
          Just rewrittenTransfer -> do
            assertEqual "the host is rewritten" (sourceHost "cache.nixos.org") rewrittenTransfer.host
            assertEqual "the start is kept" 5 rewrittenTransfer.start
            assertEqual "the activity id is kept" (Strict.Just (MkId 7)) rewrittenTransfer.activityId
    , -- INVENTORY: sourcemap.6.2
      "a public host is not treated as a proxy host" ~: do
        base <- baseState
        let state = addRunning (Host (Just "https") Nothing "cache.nixos.org") hashPart base
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "the public host is unchanged" (Just (Host (Just "https") Nothing "cache.nixos.org")) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: sourcemap.6.3
      "an https loopback host is treated as a proxy host" ~: do
        base <- baseState
        let state = addRunning (Host (Just "https") Nothing "127.0.0.1:5055") hashPart base
            rewritten = rewriteState (Map.singleton hashPart source) state
        assertEqual "the https loopback host is rewritten" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) rewritten)
    , -- INVENTORY: io.1.1
      "the resolve hook is called once per printed frame" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        printerCount <- newIORef 0
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (countingPrinter printerCount, handle))
              maintainState
              (countingResolve resolveCount)
              initial
              (delayedLineStream 200_000 [jsonLine "first", jsonLine "second"])
        resolutions <- readIORef resolveCount
        frames <- readIORef printerCount
        assertBool "at least one frame is printed" (frames >= 1)
        assertEqual "resolve runs once per printed frame" frames resolutions
    , -- INVENTORY: io.1.2
      "the printer receives the resolved state" ~: do
        initial <- testState
        recorded <- newIORef Nothing
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (hostRecordingPrinter recorded, handle))
              maintainState
              rewriteResolve
              initial
              (Stream.fromList [jsonLine "hello"])
        assertEqual "the printer sees the resolved host" (Just (sourceHost "cache.nixos.org")) =<< readIORef recorded
    , -- INVENTORY: io.1.3
      "the resolve hook runs after maintenance" ~: do
        initial <- touchedInitialState
        recorded <- newIORef Nothing
        printerCount <- newIORef 0
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (countingPrinter printerCount, handle))
              maintainState
              (touchedResolve recorded)
              initial
              (Stream.fromList [jsonLine "hello"])
        assertEqual "maintenance clears touched ids before resolve" (Just 0) =<< readIORef recorded
    , -- INVENTORY: io.1.4
      "the state kept in the frame variable stays unresolved" ~: do
        initial <- testState
        recorded <- newIORef Nothing
        returned <-
          withNullHandle \handle ->
            runJsonProcess
              (Just (hostRecordingPrinter recorded, handle))
              maintainState
              rewriteResolve
              initial
              (Stream.fromList [jsonLine "hello"])
        assertEqual "the printed host is resolved" (Just (sourceHost "cache.nixos.org")) =<< readIORef recorded
        assertEqual "the returned state stays unresolved" (hostOf (MkCacheId 0) initial) (hostOf (MkCacheId 0) returned)
    , -- INVENTORY: io.1.5
      "an unchanged resolve prints the same frame as pure" ~: do
        standardReplay <- readIntegrationReplay "standard"
        initial <- baseState
        pureOutput <- newIORef Nothing
        identityOutput <- newIORef Nothing
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (summaryRecordingPrinter pureOutput, handle))
              maintainState
              pure
              initial
              (Stream.fromList (ByteString8.lines standardReplay))
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (summaryRecordingPrinter identityOutput, handle))
              maintainState
              identityResolve
              initial
              (Stream.fromList (ByteString8.lines standardReplay))
        expectedOutput <- readIORef pureOutput
        actualOutput <- readIORef identityOutput
        assertBool "a frame was printed" (isJust expectedOutput)
        assertEqual "an unchanged resolve prints the same final frame" expectedOutput actualOutput
    , -- INVENTORY: io.1.6
      "an exception in the resolve hook escapes the run" ~: do
        initial <- baseState
        printerCount <- newIORef 0
        outcome <-
          Exception.try @Exception.SomeException do
            withNullHandle \handle ->
              void
                $ runJsonProcess
                  (Just (countingPrinter printerCount, handle))
                  maintainState
                  (\_ -> Exception.throwIO (Exception.ErrorCall "resolver boom"))
                  initial
                  (Stream.fromList [jsonLine "hello"])
        case outcome of
          Left exception -> assertBool "the resolver failure escapes" ("resolver boom" `Text.isInfixOf` Text.pack (show exception))
          Right _ -> assertFailure "the resolver failure was swallowed"
    , -- INVENTORY: io.2.1
      "interact passes its resolve argument through" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        printerCount <- newIORef 0
        withNullHandle \handle ->
          withParser @NixJSONMessage \parser ->
            void
              $ interact
                testConfig
                parser
                jsonStateUpdater
                maintainState
                (countingResolve resolveCount)
                (countingPrinter printerCount)
                jsonFinalizer
                (Right <$> Stream.fromList [jsonLine "one", jsonLine "two"])
                handle
                initial
        resolutions <- readIORef resolveCount
        assertBool "interact forwards the resolve hook" (resolutions >= 1)
    , -- INVENTORY: io.2.2
      "interact prints states produced by the resolve hook" ~: do
        initial <- testState
        recorded <- newIORef Nothing
        withNullHandle \handle ->
          withParser @NixJSONMessage \parser ->
            void
              $ interact
                testConfig
                parser
                jsonStateUpdater
                maintainState
                rewriteResolve
                (hostRecordingPrinter recorded)
                jsonFinalizer
                (Right <$> Stream.fromList [jsonLine "hello"])
                handle
                initial
        assertEqual "interact prints the resolved host" (Just (sourceHost "cache.nixos.org")) =<< readIORef recorded
    , -- INVENTORY: io.2.3
      "interact applies maintenance before resolve" ~: do
        initial <- touchedInitialState
        recorded <- newIORef Nothing
        printerCount <- newIORef 0
        withNullHandle \handle ->
          withParser @NixJSONMessage \parser ->
            void
              $ interact
                testConfig
                parser
                jsonStateUpdater
                maintainState
                (touchedResolve recorded)
                (countingPrinter printerCount)
                jsonFinalizer
                (Right <$> Stream.fromList [jsonLine "hello"])
                handle
                initial
        assertEqual "resolve sees the maintained state" (Just 0) =<< readIORef recorded
    , -- INVENTORY: io.3.1
      "the resolve hook reaches writeStateToScreen" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        printerCount <- newIORef 0
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (countingPrinter printerCount, handle))
              maintainState
              (countingResolve resolveCount)
              initial
              (Stream.fromList [jsonLine "hello"])
        resolutions <- readIORef resolveCount
        assertBool "the resolve hook is called" (resolutions >= 1)
    , -- INVENTORY: io.3.2
      "without a printer the resolve hook is never called" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        void
          $ runJsonProcess
            Nothing
            maintainState
            (countingResolve resolveCount)
            initial
            (Stream.fromList [jsonLine "hello"])
        assertEqual "resolve is never called" 0 =<< readIORef resolveCount
    , -- INVENTORY: io.3.3
      "resolve is called from the printing loop, not the parser" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        printerCount <- newIORef 0
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (countingPrinter printerCount, handle))
              maintainState
              (countingResolve resolveCount)
              initial
              (Stream.fromList (replicate 5 (jsonLine "hello")))
        resolutions <- readIORef resolveCount
        assertBool "resolve runs once or twice, not per update" (resolutions >= 1 && resolutions <= 2)
    , -- INVENTORY: io.3.4
      "each printed frame resolves once" ~: do
        initial <- baseState
        resolveCount <- newIORef 0
        printerCount <- newIORef 0
        withNullHandle \handle ->
          void
            $ runJsonProcess
              (Just (countingPrinter printerCount, handle))
              maintainState
              (countingResolve resolveCount)
              initial
              (Stream.fromList (replicate 5 (jsonLine "hello")))
        resolutions <- readIORef resolveCount
        frames <- readIORef printerCount
        assertBool "at most two frames are printed" (frames <= 2)
        assertBool "at least one frame is printed" (frames >= 1)
        assertEqual "each frame resolves once" frames resolutions
    , -- INVENTORY: io.4.1
      "testProcess with pure resolve keeps the Nix host" ~: do
        initial <- baseState
        returned <-
          runJsonProcess
            Nothing
            maintainState
            pure
            initial
            (Stream.fromList [copyPathLine "http://127.0.0.1:5055"])
        assertEqual "the download keeps the reported host" (Just (Host (Just "http") Nothing "127.0.0.1:5055")) (hostOf (MkCacheId 0) returned)
    , -- INVENTORY: io.4.2
      "integration expected states keep the reported hosts" ~: do
        standardReplay <- readIntegrationReplay "standard"
        initial <- baseState
        standardState <-
          runJsonProcess
            Nothing
            maintainState
            pure
            initial
            (Stream.fromList (ByteString8.lines standardReplay))
        assertBool "the standard replay has no running downloads" (CMap.null standardState.fullSummary.runningDownloads)
        assertBool "the standard replay has no completed downloads" (CMap.null standardState.fullSummary.completedDownloads)
        downloadInitial <- baseState
        downloadState <-
          runJsonProcess
            Nothing
            maintainState
            pure
            downloadInitial
            (Stream.fromList [copyPathLine "http://127.0.0.1:5055"])
        assertEqual "the download replay keeps the reported host" (Just (Host (Just "http") Nothing "127.0.0.1:5055")) (hostOf (MkCacheId 0) downloadState)
        assertBool "a start-only line leaves no completed download" (CMap.null downloadState.fullSummary.completedDownloads)
    , -- INVENTORY: io.4.3
      "the integration replays pass their end-state checks" ~: do
        standardReplay <- readIntegrationReplay "standard"
        failReplay <- readIntegrationReplay "fail"
        standardInitial <- baseState
        standardState <-
          runJsonProcess
            Nothing
            maintainState
            pure
            standardInitial
            (Stream.fromList (ByteString8.lines standardReplay))
        assertBool "no planned builds remain" (CSet.null standardState.fullSummary.plannedBuilds)
        assertBool "no running builds remain" (CMap.null standardState.fullSummary.runningBuilds)
        assertEqual "four builds completed" 4 (CMap.size standardState.fullSummary.completedBuilds)
        failInitial <- baseState
        failState <-
          runJsonProcess
            Nothing
            maintainState
            pure
            failInitial
            (Stream.fromList (ByteString8.lines failReplay))
        assertEqual "one build is planned" 1 (CSet.size failState.fullSummary.plannedBuilds)
        assertEqual "one build failed" 1 (CMap.size failState.fullSummary.failedBuilds)
        assertEqual "no build completed" 0 (CMap.size failState.fullSummary.completedBuilds)
        assertEqual "one build is running" 1 (CMap.size failState.fullSummary.runningBuilds)
    , -- discovery.1 Resolver construction with a no-proxy TLS manager

      -- INVENTORY: discovery.1.1
      "newResolver issues no request" ~: withStub 0 \stub -> do
        void newResolver
        Concurrent.threadDelay 300_000
        assertEqual "no request is logged" [] =<< stub.stubLog
    , -- INVENTORY: discovery.1.2
      "separate resolvers keep separate pollers" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        firstResolver <- newResolver
        secondResolver <- newResolver
        void (firstResolver state)
        void (secondResolver state)
        Concurrent.threadDelay 1_500_000
        requests <- infoRequests stub
        assertEqual "one poll per resolver" 2 (length requests)
    , -- INVENTORY: discovery.1.3
      "non-loopback state is unchanged" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "10.0.0.1") hashPart base
        resolver <- newResolver
        resolved <- resolver state
        assertEqual "state is unchanged" state resolved
    , -- INVENTORY: discovery.1.4
      "resolver ignores proxy environment variables" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- withProxyEnvironment newResolver
        void (resolver state)
        Concurrent.threadDelay 600_000
        requests <- infoRequests stub
        assertEqual "the loopback host is reached directly" 1 (length requests)
    , -- INVENTORY: discovery.1.5
      "separate resolvers keep separate source maps" ~: withStub 0 \stubA -> withStub 300_000 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        stubB.stubSetSourcesBody (sourceLine secondHash "https://b.example.org")
        resolverA <- newResolver
        resolverB <- newResolver
        firstOnly <- stateWithDownloads [(loopbackHost stubA.stubPort, firstHash)]
        secondOnly <- stateWithDownloads [(loopbackHost stubB.stubPort, secondHash)]
        void (resolverA firstOnly)
        waitForRequestCount stubA 2
        void (resolverB secondOnly)
        waitForRequestCount stubB 2
        Concurrent.threadDelay 800_000
        combined <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, secondHash)
            ]
        afterA <- resolverA combined
        assertEqual "the first map rewrites its own host" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) afterA)
        assertEqual "the second host is untouched" (Just (loopbackHost stubB.stubPort)) (hostOf (MkCacheId 1) afterA)
        afterB <- resolverB afterA
        assertEqual "the second map rewrites its own host" (Just (sourceHost "b.example.org")) (hostOf (MkCacheId 1) afterB)
        assertEqual "the first rewrite survives" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) afterB)
    , -- discovery.2 Poller bookkeeping state

      -- INVENTORY: discovery.2.1
      "the first frame records a poller and applies no map" ~: withStub 500_000 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine hashPart "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        resolved <- resolver state
        assertEqual "the first frame keeps the proxy host" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolved)
        Concurrent.threadDelay 1_200_000
        requests <- infoRequests stub
        assertEqual "exactly one info GET" 1 (length requests)
    , -- INVENTORY: discovery.2.2
      "a host that leaves the state is not polled again" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        empty <- baseState
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 200_000
        void (resolver empty)
        Concurrent.threadDelay 200_000
        void (resolver state)
        Concurrent.threadDelay 1_000_000
        requests <- infoRequests stub
        assertEqual "one info GET" 1 (length requests)
    , -- INVENTORY: discovery.2.3
      "maps for two hosts apply on the next frame" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        stubB.stubSetSourcesBody (sourceLine secondHash "https://b.example.org")
        state <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, secondHash)
            ]
        resolver <- newResolver
        void (resolver state)
        resolved <-
          awaitHosts
            resolver
            state
            [ (MkCacheId 0, sourceHost "a.example.org")
            , (MkCacheId 1, sourceHost "b.example.org")
            ]
        assertEqual "the first host is rewritten" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) resolved)
        assertEqual "the second host is rewritten" (Just (sourceHost "b.example.org")) (hostOf (MkCacheId 1) resolved)
    , -- INVENTORY: discovery.2.4
      "bookkeeping updates are atomic" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody "hello\n"
        stubB.stubSetInfoBody "hello\n"
        stateA <- stateWithDownloads [(loopbackHost stubA.stubPort, hashPart)]
        stateB <- stateWithDownloads [(loopbackHost stubB.stubPort, secondHash)]
        resolver <- newResolver
        void (Concurrent.forkIO (repeatResolve 20 resolver stateA))
        void (Concurrent.forkIO (repeatResolve 20 resolver stateB))
        Concurrent.threadDelay 1_500_000
        requestsA <- infoRequests stubA
        requestsB <- infoRequests stubB
        assertEqual "one poller for the first host" 1 (length requestsA)
        assertEqual "one poller for the second host" 1 (length requestsB)
    , -- discovery.3 One poller per newly seen loopback download host

      -- INVENTORY: discovery.3.1
      "loopback running download starts exactly one poller" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 800_000
        requests <- infoRequests stub
        assertEqual "one info GET" 1 (length requests)
    , -- INVENTORY: discovery.3.2
      "two loopback hosts start one poller each" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody "hello\n"
        stubB.stubSetInfoBody "hello\n"
        state <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, secondHash)
            ]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 800_000
        requestsA <- infoRequests stubA
        requestsB <- infoRequests stubB
        assertEqual "one info GET for the first host" 1 (length requestsA)
        assertEqual "one info GET for the second host" 1 (length requestsB)
    , -- INVENTORY: discovery.3.3
      "a host seen in consecutive frames starts one poller" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        replicateM_ 3 do
          void (resolver state)
          Concurrent.threadDelay 100_000
        Concurrent.threadDelay 500_000
        requests <- infoRequests stub
        assertEqual "one info GET" 1 (length requests)
    , -- INVENTORY: discovery.3.4
      "a non-loopback host starts no poller" ~: withStub 0 \stub -> do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "10.0.0.1") hashPart base
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 500_000
        assertEqual "no request is logged" [] =<< stub.stubLog
    , -- INVENTORY: discovery.3.5
      "a completed download starts a poller" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        base <- baseState
        let state = addCompleted (loopbackHost stub.stubPort) hashPart base
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 800_000
        requests <- infoRequests stub
        assertEqual "one info GET" 1 (length requests)
    , -- INVENTORY: discovery.3.6
      "resolve returns before the poll completes" ~: withStub 2_000_000 \stub -> do
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        started <- Clock.getMonotonicTime
        void (resolver state)
        finished <- Clock.getMonotonicTime
        assertBool "resolve returns before the delayed answer" (finished - started < 1.0)
        waitForRequestCount stub 1
    , -- INVENTORY: discovery.3.7
      "a host that leaves the state keeps its poller" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine hashPart "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        empty <- baseState
        resolver <- newResolver
        void (resolver state)
        replicateM_ 22 do
          void (resolver empty)
          Concurrent.threadDelay 100_000
        requests <- infoRequests stub
        let times = map fst requests
        assertBool "three polls happen" (length times >= 3)
        assertBool
          "polls are about one second apart"
          (all (\(previous, next) -> next - previous > 0.5 && next - previous < 2.0) (zip times (drop 1 times)))
    , -- discovery.4 Resolved source maps applied to the NOM state each frame

      -- INVENTORY: discovery.4.1
      "no stored map leaves the state unchanged" ~: do
        port <- closedPort
        base <- baseState
        let state = addRunning (loopbackHost port) hashPart base
        resolver <- newResolver
        resolved <- resolver state
        assertEqual "state is unchanged" state resolved
    , -- INVENTORY: discovery.4.2
      "a stored map rewrites a download from a later frame" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        base <- baseState
        let earlier = addRunning (loopbackHost stub.stubPort) secondHash base
            later = addRunning (loopbackHost stub.stubPort) firstHash base
        resolver <- newResolver
        void (resolver earlier)
        resolved <- awaitHosts resolver later [(MkCacheId 0, sourceHost "cache.nixos.org")]
        assertEqual "the later download is rewritten" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) resolved)
    , -- INVENTORY: discovery.4.3
      "a map stored for one host applies to another host" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        base <- baseState
        let earlier = addRunning (loopbackHost stubA.stubPort) firstHash base
            later = addRunning (loopbackHost stubB.stubPort) firstHash base
        resolver <- newResolver
        void (resolver earlier)
        resolved <- awaitHosts resolver later [(MkCacheId 0, sourceHost "a.example.org")]
        assertEqual "the other host's map applies" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) resolved)
    , -- INVENTORY: discovery.4.4
      "downloads are rewritten by their own path hashes" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <-
          stateWithDownloads
            [ (loopbackHost stub.stubPort, firstHash)
            , (loopbackHost stub.stubPort, secondHash)
            ]
        resolver <- newResolver
        void (resolver state)
        resolved <- awaitHosts resolver state [(MkCacheId 0, sourceHost "cache.nixos.org")]
        assertEqual "the mapped download is rewritten" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) resolved)
        assertEqual "the other download keeps the proxy host" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 1) resolved)
    , -- INVENTORY: discovery.4.5
      "an empty map does not clear another host's map" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        state <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, secondHash)
            ]
        resolver <- newResolver
        void (resolver state)
        mapped <- awaitHosts resolver state [(MkCacheId 0, sourceHost "a.example.org")]
        assertEqual "the first host is rewritten" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) mapped)
        waitForRequestCount stubB 2
        Concurrent.threadDelay 300_000
        resolved <- resolver state
        assertEqual "the first host is still rewritten" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) resolved)
        assertEqual "the second host keeps its proxy" (Just (loopbackHost stubB.stubPort)) (hostOf (MkCacheId 1) resolved)
    , -- INVENTORY: discovery.4.6
      "the map for the smaller base URL wins" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        stubB.stubSetSourcesBody (sourceLine firstHash "https://b.example.org")
        state <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, firstHash)
            ]
        let baseA = "http://127.0.0.1:" <> Text.pack (show stubA.stubPort)
            baseB = "http://127.0.0.1:" <> Text.pack (show stubB.stubPort)
            winner = if baseA < baseB then sourceHost "a.example.org" else sourceHost "b.example.org"
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stubA 2
        waitForRequestCount stubB 2
        Concurrent.threadDelay 300_000
        resolved <- resolver state
        assertEqual "the smaller base URL wins" (Just winner) (hostOf (MkCacheId 0) resolved)
    , -- discovery.5 Poll cycle fetches info then sources and stores the parsed map

      -- INVENTORY: discovery.5.1
      "info is fetched before sources" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        targets <- requestTargets stub
        assertEqual "info then sources" [infoPath, sourcesPath] (take 2 targets)
    , -- INVENTORY: discovery.5.2
      "the sources body is applied on the following frame" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, firstHash)]
        resolver <- newResolver
        void (resolver state)
        resolved <- awaitHosts resolver state [(MkCacheId 0, sourceHost "cache.nixos.org")]
        assertEqual "the mapped host replaces the proxy" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) resolved)
    , -- INVENTORY: discovery.5.3
      "an empty sources body stores an empty map" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        Concurrent.threadDelay 300_000
        resolved <- resolver state
        assertEqual "the host is untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolved)
        requests <- sourcesRequests stub
        assertBool "a sources GET happened" (not (null requests))
    , -- INVENTORY: discovery.5.4
      "a later cycle replaces the stored map" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, firstHash)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 1_200_000
        stub.stubSetSourcesBody (sourceLine firstHash "https://b.example.org")
        Concurrent.threadDelay 1_500_000
        resolved <- resolver state
        assertEqual "the new map replaces the old one" (Just (sourceHost "b.example.org")) (hostOf (MkCacheId 0) resolved)
    , -- INVENTORY: discovery.5.5
      "a successful cycle repeats after one second" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 2_500_000
        requests <- infoRequests stub
        let times = map fst requests
        assertBool "at least two info GETs" (length times >= 2)
        assertBool
          "the second info GET is about one second later"
          (case take 2 times of [earlier, later] -> later - earlier > 0.5 && later - earlier < 2.0; _ -> False)
    , -- INVENTORY: discovery.5.6
      "maps are keyed by base URL" ~: withStub 0 \stubA -> withStub 0 \stubB -> do
        stubA.stubSetInfoBody proxyInfo
        stubA.stubSetSourcesBody (sourceLine firstHash "https://a.example.org")
        stubB.stubSetInfoBody proxyInfo
        stubB.stubSetSourcesBody (sourceLine secondHash "https://b.example.org")
        state <-
          stateWithDownloads
            [ (loopbackHost stubA.stubPort, firstHash)
            , (loopbackHost stubB.stubPort, secondHash)
            ]
        resolver <- newResolver
        void (resolver state)
        resolved <-
          awaitHosts
            resolver
            state
            [ (MkCacheId 0, sourceHost "a.example.org")
            , (MkCacheId 1, sourceHost "b.example.org")
            ]
        assertEqual "the first host uses its own map" (Just (sourceHost "a.example.org")) (hostOf (MkCacheId 0) resolved)
        assertEqual "the second host uses its own map" (Just (sourceHost "b.example.org")) (hostOf (MkCacheId 1) resolved)
    , -- discovery.6 Failed info poll retries, a sources failure refreshes

      -- INVENTORY: discovery.6.1
      "a failed info request retries after five seconds" ~: withStub 0 \stub -> do
        stub.stubSetInfoStatus 500
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        resolved <- resolver state
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        let times = map fst requests
        assertEqual "two info GETs" 2 (length times)
        assertBool
          "about five seconds apart"
          (case times of [earlier, later] -> later - earlier > 3.0 && later - earlier < 8.0; _ -> False)
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "the host is untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolved)
        resolvedLater <- resolver state
        assertEqual "the host is still untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolvedLater)
    , -- INVENTORY: discovery.6.2
      "info without proxy markers retries without fetching sources" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "two info GETs" 2 (length requests)
        assertEqual "no sources GET" 0 (length sources)
    , -- INVENTORY: discovery.6.3
      "a sources 500 stores an empty map and info keeps refreshing" ~: withStub 100_000 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, firstHash)]
        resolver <- newResolver
        unresolved <- resolver state
        assertEqual "the first frame has no map yet" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) unresolved)
        Concurrent.threadDelay 1_600_000
        mapped <- resolver state
        assertEqual "the earlier 200 answers stored the map" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) mapped)
        stub.stubSetSourcesStatus 500
        Concurrent.threadDelay 1_400_000
        afterFailure <- resolver state
        assertEqual "the empty 500 body replaces the map" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) afterFailure)
        requests <- infoRequests stub
        assertBool "three info GETs in three seconds" (length requests >= 3)
    , -- INVENTORY: discovery.6.4
      "a host that starts failing keeps its last good map" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, firstHash)]
        resolver <- newResolver
        void (resolver state)
        mapped <- awaitHosts resolver state [(MkCacheId 0, sourceHost "cache.nixos.org")]
        assertEqual "the good map applies" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) mapped)
        stub.stubSetInfoStatus 500
        Concurrent.threadDelay 1_500_000
        afterFailure <- resolver state
        assertEqual "the map survives the failure" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) afterFailure)
        Concurrent.threadDelay 1_500_000
        muchLater <- resolver state
        assertEqual "the map still survives" (Just (sourceHost "cache.nixos.org")) (hostOf (MkCacheId 0) muchLater)
    , -- INVENTORY: discovery.6.5
      "retries continue indefinitely" ~: withStub 0 \stub -> do
        stub.stubSetInfoStatus 500
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 16_000_000
        requests <- infoRequests stub
        let times = map fst requests
        assertBool "four info GETs" (length times >= 4)
        assertBool
          "about five seconds apart"
          (all (\(earlier, later) -> later - earlier > 3.0 && later - earlier < 8.0) (zip times (drop 1 times)))
    , -- discovery.7 HTTP failure is swallowed as a missing body

      -- INVENTORY: discovery.7.1
      "a refused connection yields a missing body" ~: do
        port <- closedPort
        base <- baseState
        let state = addRunning (loopbackHost port) hashPart base
        resolver <- newResolver
        resolved <- resolver state
        assertEqual "state is unchanged" state resolved
    , -- INVENTORY: discovery.7.2
      "a 404 info response yields a missing body" ~: withStub 0 \stub -> do
        stub.stubSetInfoStatus 404
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        resolved <- resolver state
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "two info GETs" 2 (length requests)
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "the host is untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolved)
        resolvedLater <- resolver state
        assertEqual "the host is still untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolvedLater)
    , -- INVENTORY: discovery.7.3
      "a stalled response yields a missing body" ~: withStub 5_000_000 \stub -> do
        stub.stubSetInfoBody proxyInfo
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 7_500_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        let times = map fst requests
        assertEqual "two info GETs" 2 (length times)
        assertBool
          "about six seconds apart"
          (case times of [earlier, later] -> later - earlier > 3.0 && later - earlier < 10.0; _ -> False)
        assertEqual "no sources GET" 0 (length sources)
    , -- INVENTORY: discovery.7.4
      "a malformed host URL yields a missing body" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "127.0.0.1:notaport") hashPart base
        resolver <- newResolver
        resolved <- resolver state
        assertEqual "state is unchanged" state resolved
    , -- INVENTORY: discovery.7.5
      "an empty 200 info body yields an empty body" ~: withStub 0 \stub -> do
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        resolved <- resolver state
        Concurrent.threadDelay 2_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "one info GET" 1 (length requests)
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "the host is untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolved)
        resolvedLater <- resolver state
        assertEqual "the host is still untouched" (Just (loopbackHost stub.stubPort)) (hostOf (MkCacheId 0) resolvedLater)
    , -- discovery.8 Poll timing constants

      -- INVENTORY: discovery.8.1
      "the request timeout is one second" ~: do
        contents <- readCacheDiscoverySource
        assertBool "requestTimeout is 1_000_000" ("requestTimeout = 1_000_000" `Text.isInfixOf` contents)
    , -- INVENTORY: discovery.8.2
      "the refresh interval is one second" ~: do
        contents <- readCacheDiscoverySource
        assertBool "refreshInterval is 1_000_000" ("refreshInterval = 1_000_000" `Text.isInfixOf` contents)
    , -- INVENTORY: discovery.8.3
      "the retry interval is five seconds" ~: do
        contents <- readCacheDiscoverySource
        assertBool "retryInterval is 5_000_000" ("retryInterval = 5_000_000" `Text.isInfixOf` contents)
    , -- protocol.1 Info endpoint path

      -- INVENTORY: protocol.1.1
      "the poller requests exactly the info path" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 1
        targets <- requestTargets stub
        assertEqual "single info target" (Just "/.nix-cache-discovery/v1/info") (listToMaybe targets)
    , -- INVENTORY: protocol.1.2
      "the info path is joined without a doubled slash" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        state <-
          stateWithDownloads
            [(Host Nothing Nothing ("127.0.0.1:" <> Text.pack (show stub.stubPort) <> "/"), hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 1
        targets <- requestTargets stub
        assertEqual "single slash" (Just "/.nix-cache-discovery/v1/info") (listToMaybe targets)
        assertBool "no doubled slash" (not (any (Text.isInfixOf "//") targets))
    , -- INVENTORY: protocol.1.3
      "the info request carries no query string" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 1
        targets <- map snd <$> infoRequests stub
        assertEqual "one info target" 1 (length targets)
        assertBool "no query string" (not (any (Text.isInfixOf "?") targets))
    , -- protocol.2 Sources endpoint path

      -- INVENTORY: protocol.2.1
      "the poller requests exactly the sources path" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        targets <- requestTargets stub
        assertEqual "single sources target" (Just "/.nix-cache-discovery/v1/sources") (listToMaybe (drop 1 targets))
    , -- INVENTORY: protocol.2.2
      "sources is requested only after the info body is accepted" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "hello\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "two info GETs" 2 (length requests)
    , -- INVENTORY: protocol.2.3
      "the sources request carries no query string" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        targets <- map snd <$> sourcesRequests stub
        assertEqual "one sources target" 1 (length targets)
        assertBool "no query string" (not (any (Text.isInfixOf "?") targets))
    , -- protocol.3 Proxy info body recognition

      -- INVENTORY: protocol.3.1
      "both markers in one body are accepted" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody proxyInfo
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        sources <- sourcesRequests stub
        assertEqual "sources is requested once" 1 (length sources)
    , -- INVENTORY: protocol.3.2
      "a body with version=1 and no id= is rejected" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "version=1\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "two info GETs" 2 (length requests)
    , -- INVENTORY: protocol.3.3
      "a body with id= and no version=1 is rejected" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "id=abc\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "two info GETs" 2 (length requests)
    , -- INVENTORY: protocol.3.4
      "an empty info body is rejected" ~: withStub 0 \stub -> do
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "two info GETs" 2 (length requests)
    , -- INVENTORY: protocol.3.5
      "markers split across lines are accepted" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "id=abc\nversion=1\n"
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        sources <- sourcesRequests stub
        assertEqual "sources is requested once" 1 (length sources)
    , -- INVENTORY: protocol.3.6
      "version=10 is accepted through the version=1 prefix" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "version=10\nid=abc\n"
        stub.stubSetSourcesBody (sourceLine firstHash "https://cache.nixos.org")
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        waitForRequestCount stub 2
        sources <- sourcesRequests stub
        assertEqual "sources is requested once" 1 (length sources)
    , -- INVENTORY: protocol.3.7
      "the marker match is case sensitive" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody "VERSION=1\nid=abc\n"
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "no sources GET" 0 (length sources)
        assertEqual "two info GETs" 2 (length requests)
    -- INVENTORY: protocol.3.8
    , "a non-UTF-8 body is retried like a non-proxy body" ~: withStub 0 \stub -> do
        stub.stubSetInfoBody (ByteString.pack [0xff, 0xfe])
        state <- stateWithDownloads [(loopbackHost stub.stubPort, hashPart)]
        resolver <- newResolver
        void (resolver state)
        Concurrent.threadDelay 6_000_000
        requests <- infoRequests stub
        sources <- sourcesRequests stub
        assertEqual "two info GETs" 2 (length requests)
        assertEqual "no sources GET" 0 (length sources)
    , -- protocol.4 Proxy base URL from a loopback host

      -- INVENTORY: protocol.4.2
      "a host without a protocol defaults to http" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "127.0.0.1:5055") hashPart base
        assertEqual "http candidate" (Set.singleton "http://127.0.0.1:5055") (proxyCandidates state)
    , -- INVENTORY: protocol.4.3
      "an https host keeps its protocol" ~: do
        base <- baseState
        let state = addRunning (Host (Just "https") Nothing "127.0.0.1:5055") hashPart base
        assertEqual "https candidate" (Set.singleton "https://127.0.0.1:5055") (proxyCandidates state)
    , -- INVENTORY: protocol.4.4
      "trailing slashes are stripped" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "127.0.0.1:5055///") hashPart base
        assertEqual "slash free candidate" (Set.singleton "http://127.0.0.1:5055") (proxyCandidates state)
    , -- INVENTORY: protocol.4.5
      "an IPv6 loopback host keeps its brackets" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "[::1]:5055") hashPart base
        assertEqual "bracketed candidate" (Set.singleton "http://[::1]:5055") (proxyCandidates state)
    , -- INVENTORY: protocol.4.6
      "a host without a port yields no port" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "127.0.0.1") hashPart base
        assertEqual "port free candidate" (Set.singleton "http://127.0.0.1") (proxyCandidates state)
    , -- protocol.5 Loopback host prefix set

      -- INVENTORY: protocol.5.1
      "127.0.0.0/8 outside 127.0.0.1 is loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "127.1.2.3:5055") hashPart base
        assertEqual "loopback candidate" (Set.singleton "http://127.1.2.3:5055") (proxyCandidates state)
    , -- INVENTORY: protocol.5.2
      "localhost is loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "localhost:5000") hashPart base
        assertEqual "localhost candidate" (Set.singleton "http://localhost:5000") (proxyCandidates state)
    , -- INVENTORY: protocol.5.3
      "[::1] and ::1 are loopback" ~: do
        base <- baseState
        let bracketed = addRunning (Host Nothing Nothing "[::1]:5055") hashPart base
            bare = addRunning (Host Nothing Nothing "::1") hashPart base
        assertEqual "bracketed candidate" (Set.singleton "http://[::1]:5055") (proxyCandidates bracketed)
        assertEqual "bare candidate" (Set.singleton "http://::1") (proxyCandidates bare)
    , -- INVENTORY: protocol.5.4
      "a host starting with localhost is loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "localhost.evil:5000") hashPart base
        assertEqual "prefix match candidate" (Set.singleton "http://localhost.evil:5000") (proxyCandidates state)
    , -- INVENTORY: protocol.5.5
      "a 10.x host is not loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "10.0.0.1") hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- INVENTORY: protocol.5.6
      "a 192.168.x host is not loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "192.168.1.1") hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- INVENTORY: protocol.5.7
      "uppercase LOCALHOST is not loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "LOCALHOST") hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- INVENTORY: protocol.5.8
      "a host starting with ::2 is not loopback" ~: do
        base <- baseState
        let state = addRunning (Host Nothing Nothing "::2") hashPart base
        assertEqual "no candidate" Set.empty (proxyCandidates state)
    , -- package.1 Library exposes the CacheDiscovery modules

      -- INVENTORY: package.1.3
      "a single consumer imports both library modules" ~: do
        void CacheDiscovery.newResolver
        assertEqual "the source map parser is available" Map.empty (SourceMap.parseSourceMap "")
    , -- INVENTORY: package.1.4
      "the exposed module list matches the library files" ~: do
        cabalFile <- readRepoText ["nix-output-monitor.cabal", "nix-output-monitor/nix-output-monitor.cabal"]
        libraryDirectory <- readRepoDirectory ["lib", "nix-output-monitor/lib"]
        modules <- moduleNamesUnder libraryDirectory libraryDirectory
        assertEqual
          "the exposed modules are exactly the library files"
          (Set.fromList (exposedModules cabalFile))
          (Set.fromList modules)
    , -- package.2 http-client and http-client-tls in the shared build-depends

      -- INVENTORY: package.2.3
      "the executable inherits both HTTP dependencies" ~: do
        cabalFile <- readRepoText ["nix-output-monitor.cabal", "nix-output-monitor/nix-output-monitor.cabal"]
        let sharedDependencies = cabalField cabalFile "common common-config" "build-depends:"
            executableImports = cabalField cabalFile "executable nom" "import:"
            exesImports = cabalField cabalFile "common exes" "import:"
        assertBool "http-client is in the shared build-depends" ("http-client" `elem` sharedDependencies)
        assertBool "http-client-tls is in the shared build-depends" ("http-client-tls" `elem` sharedDependencies)
        assertBool "the executable imports the exes stanza" ("exes" `elem` executableImports)
        assertBool "the exes stanza imports common-config" ("common-config" `elem` exesImports)
    , -- INVENTORY: package.2.4
      "the test suite inherits both HTTP dependencies" ~: do
        cabalFile <- readRepoText ["nix-output-monitor.cabal", "nix-output-monitor/nix-output-monitor.cabal"]
        let sharedDependencies = cabalField cabalFile "common common-config" "build-depends:"
            suiteImports = cabalField cabalFile "test-suite cache-discovery-tests" "import:"
            testsImports = cabalField cabalFile "common tests" "import:"
        assertBool "http-client is in the shared build-depends" ("http-client" `elem` sharedDependencies)
        assertBool "http-client-tls is in the shared build-depends" ("http-client-tls" `elem` sharedDependencies)
        assertBool "the suite imports the tests stanza" ("tests" `elem` suiteImports)
        assertBool "the tests stanza imports exes" ("exes" `elem` testsImports)
    , -- test.1 Suite exits nonzero when any HUnit case fails

      -- INVENTORY: test.1.2
      "a failing case yields a failing suite exit" ~: do
        counts <- runTestTT (test (assertEqual "deliberate failure" (1 :: Int) 2))
        assertEqual "one failure is counted" 1 (failures counts)
        assertBool "the suite exits nonzero" (not (suiteSucceeds counts))
    , -- INVENTORY: test.1.3
      "an erroring case yields a failing suite exit" ~: do
        counts <- runTestTT (test (Exception.throwIO (Exception.ErrorCall "boom") :: IO ()))
        assertEqual "one error is counted" 1 (errors counts)
        assertBool "the suite exits nonzero" (not (suiteSucceeds counts))
    , -- INVENTORY: test.1.5
      "a failing case is reported with its label" ~: do
        output <-
          captureStderr do
            void (runTestTT (test ("parse source map label" ~: assertEqual "deliberate failure" (1 :: Int) 2)))
        assertBool "the label is printed" ("parse source map label" `ByteString.isInfixOf` output)
    ]

-- INVENTORY: test.1.1
main :: IO ()
main = do
  counts <- runTestTT . test =<< tests
  if suiteSucceeds counts then exitSuccess else exitFailure
