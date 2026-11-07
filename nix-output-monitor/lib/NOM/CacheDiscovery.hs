module NOM.CacheDiscovery (newResolver) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Network.HTTP.Client (Manager, httpLbs, managerResponseTimeout, managerSetProxy, newManager, noProxy, parseRequest, responseBody, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import NOM.CacheDiscovery.SourceMap (SourceMap, parseSourceMap, proxyCandidates, rewriteState)
import NOM.State (NOMState)
import Relude

data PollState = MkPollState
  { startedPollers :: Set Text
  , sourceMaps :: Map Text SourceMap
  }

initialPollState :: PollState
initialPollState = MkPollState mempty mempty

newResolver :: IO (NOMState -> IO NOMState)
newResolver = do
  pollState <- STM.newTVarIO initialPollState
  let settings = managerSetProxy noProxy (tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro requestTimeout})
  manager <- newManager settings
  pure (resolveWith manager pollState)

requestTimeout :: Int
requestTimeout = 1_000_000

refreshInterval :: Int
refreshInterval = 1_000_000

retryInterval :: Int
retryInterval = 5_000_000

infoPath :: Text
infoPath = "/.nix-cache-discovery/v1/info"

sourcesPath :: Text
sourcesPath = "/.nix-cache-discovery/v1/sources"

resolveWith :: Manager -> STM.TVar PollState -> NOMState -> IO NOMState
resolveWith manager pollState nomState = do
  newPollers <- STM.atomically do
    current <- STM.readTVar pollState
    let newOnes = proxyCandidates nomState `Set.difference` current.startedPollers
    when (not (Set.null newOnes)) do
      STM.modifyTVar' pollState \old -> old{startedPollers = old.startedPollers <> newOnes}
    pure newOnes
  forM_ (Set.toList newPollers) \base -> void $ Concurrent.forkIO (pollLoop manager pollState base)
  current <- STM.atomically (STM.readTVar pollState)
  let sources = Map.unions (Map.elems current.sourceMaps)
  pure (if Map.null sources then nomState else rewriteState sources nomState)

pollLoop :: Manager -> STM.TVar PollState -> Text -> IO ()
pollLoop manager pollState base = do
  info <- fetchBody manager (base <> infoPath)
  case info of
    Just body | isProxyInfo body -> do
      sources <- fetchBody manager (base <> sourcesPath)
      forM_ sources \sourcesBody ->
        STM.atomically $ STM.modifyTVar' pollState \old ->
          old{sourceMaps = Map.insert base (parseSourceMap sourcesBody) old.sourceMaps}
      Concurrent.threadDelay refreshInterval
    _ -> Concurrent.threadDelay retryInterval
  pollLoop manager pollState base

isProxyInfo :: ByteString -> Bool
isProxyInfo body =
  let text = decodeUtf8 body
   in "version=1" `Text.isInfixOf` text && "id=" `Text.isInfixOf` text

fetchBody :: Manager -> Text -> IO (Maybe ByteString)
fetchBody manager url = do
  result <- Exception.try @Exception.SomeException do
    request <- parseRequest (toString url)
    response <- httpLbs request manager
    pure (LazyByteString.toStrict (responseBody response))
  pure (either (const Nothing) Just result)
