module NOM.CacheDiscovery.SourceMap (
  SourceMap,
  parseSourceMap,
  proxyCandidates,
  rewriteState,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import NOM.Builds (Host (..), HostContext (WithContext), StorePath (..), parseHost)
import NOM.State (DependencySummary (..), DerivationInfo (..), NOMState (..), StorePathId, StorePathInfo (..), StorePathMap, TransferInfo (..))
import NOM.State.CacheId.Map qualified as CMap
import Relude

type SourceMap = Map Text Text

parseSourceMap :: ByteString -> SourceMap
parseSourceMap = Map.fromList . mapMaybe parseLine . Text.lines . decodeUtf8
 where
  parseLine line = case Text.words line of
    [hashPart, source] | Text.length hashPart == 32 -> Just (hashPart, source)
    _ -> Nothing

proxyCandidates :: NOMState -> Set Text
proxyCandidates nomState = Set.fromList $ mapMaybe proxyBase (downloadHosts nomState)

downloadHosts :: NOMState -> [Host WithContext]
downloadHosts nomState =
  [ transferInfo.host
  | (_, transferInfo) <- CMap.toList nomState.fullSummary.runningDownloads
  ]
    <> [ transferInfo.host
       | (_, transferInfo) <- CMap.toList nomState.fullSummary.completedDownloads
       ]

proxyBase :: Host WithContext -> Maybe Text
proxyBase = \case
  Host protocol _ rawHostName
    | isLoopbackHost hostName -> Just (fromMaybe "http" protocol <> "://" <> hostName)
   where
    hostName = Text.dropWhileEnd (== '/') rawHostName
  _ -> Nothing

isLoopbackHost :: Text -> Bool
isLoopbackHost hostName = any (`Text.isPrefixOf` hostName) ["127.", "localhost", "[::1]", "::1"]

rewriteState :: SourceMap -> NOMState -> NOMState
rewriteState sources nomState
  | Map.null sources = nomState
  | otherwise =
      nomState
        { fullSummary = rewriteSummary sources nomState.storePathInfos nomState.fullSummary
        , derivationInfos = fmap rewriteDerivation nomState.derivationInfos
        }
 where
  rewriteDerivation derivationInfo =
    derivationInfo{dependencySummary = rewriteSummary sources nomState.storePathInfos derivationInfo.dependencySummary}

rewriteSummary :: SourceMap -> StorePathMap StorePathInfo -> DependencySummary -> DependencySummary
rewriteSummary sources storePaths summary =
  summary
    { runningDownloads = rewriteTransferMap sources storePaths summary.runningDownloads
    , completedDownloads = rewriteTransferMap sources storePaths summary.completedDownloads
    }

rewriteTransferMap :: SourceMap -> StorePathMap StorePathInfo -> StorePathMap (TransferInfo a) -> StorePathMap (TransferInfo a)
rewriteTransferMap sources storePaths = storePathMapFromList . fmap rewrite . CMap.toList
 where
  rewrite (pathId, transferInfo) = (pathId, setHost (resolveHost pathId transferInfo.host) transferInfo)
  resolveHost pathId oldHost
    | isProxyHost oldHost
    , Just storePathInfo <- CMap.lookup pathId storePaths
    , Just source <- Map.lookup storePathInfo.name.hash sources =
        parseHost source
    | otherwise = oldHost

isProxyHost :: Host WithContext -> Bool
isProxyHost = isJust . proxyBase

setHost :: Host WithContext -> TransferInfo a -> TransferInfo a
setHost newHost transferInfo = transferInfo{host = newHost}

storePathMapFromList :: [(StorePathId, value)] -> StorePathMap value
storePathMapFromList = foldl' (\accumulator (pathId, value) -> CMap.insert pathId value accumulator) mempty
