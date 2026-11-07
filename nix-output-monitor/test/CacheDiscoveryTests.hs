module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import NOM.Builds (Host (..), StorePath (..))
import NOM.CacheDiscovery.SourceMap (parseSourceMap, proxyCandidates, rewriteState)
import NOM.State (DependencySummary (..), NOMState (..), StorePathInfo (..), TransferInfo (..))
import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.Initial (initialStateFromBuildPlatform)
import Relude
import Test.HUnit (
  Counts (errors, failures),
  Test,
  Testable (test),
  assertEqual,
  runTestTT,
  (~:),
 )

hashPart :: Text
hashPart = "0123456789abcdef0123456789abcdef"

source :: Text
source = "https://cache.nixos.org"

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

tests :: IO [Test]
tests = do
  nomState <- testState
  pure
    [ "parse source map" ~: do
        let input = encodeUtf8 (hashPart <> " " <> source <> "\nnot a source line\n")
        assertEqual "source map is parsed" (Map.singleton hashPart source) (parseSourceMap input)
    , "detect proxy candidates" ~: do
        assertEqual
          "loopback download host is a proxy candidate"
          (Set.singleton "http://127.0.0.1:5055")
          (proxyCandidates nomState)
    , "rewrite download host" ~: do
        let rewritten = rewriteState (Map.singleton hashPart source) nomState
        assertEqual
          "source host replaces the proxy host"
          (Just (Host (Just "https") Nothing "cache.nixos.org"))
          (fmap (.host) (CMap.lookup (MkCacheId 0) rewritten.fullSummary.runningDownloads))
    ]

main :: IO ()
main = do
  counts <- runTestTT . test =<< tests
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
