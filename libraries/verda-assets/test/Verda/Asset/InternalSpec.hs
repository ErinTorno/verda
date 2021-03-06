{-# LANGUAGE RecordWildCards #-}

module Verda.Asset.InternalSpec where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Default
import           Data.Dynamic
import qualified Data.Foldable           as F
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Sequence           (Seq((:<|)), (|>))
import qualified Data.Sequence           as Seq
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Data.Vector             as Vec
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Test.Hspec
import qualified Test.HUnit              as HUnit

import qualified Verda.Asset.Path        as Path
import           Verda.Asset.Internal
import           Verda.Asset.Types

-----------
-- Tests --
-----------

spec :: Spec
spec =
    context "Verda.Asset.Internal" $ do
        assetStatusTest
        handleLoadResultTest
        insertAssetLoaderTest
        labeledTest
        loadHandleTest
        loadSetSummaryTest
        updateWaitingTest
        withResourceTest

assetStatusTest :: Spec
assetStatusTest =
    describe "assetStatus" $ do
        it "should return NotLoaded for handles outside of loaded range" $ do
            assets <- emptyAssets def
            assetStatus assets (Handle (-1)) `shouldReturn` NotLoaded
            assetStatus assets (Handle 123)  `shouldReturn` NotLoaded
        it "should return for a valid index" $ do
            assets <- emptyAssets def
            writeAssetStatus 0 Loaded assets
            assetStatus assets (Handle 0) `shouldReturn` Loaded

handleLoadResultTest :: Spec 
handleLoadResultTest =
    describe "handleLoadResult" $ do
        let intAsset   = 123 :: Int
            dynAsset   = toDyn intAsset
            handle     = Handle 0
            makeAssets = emptyAssets def >>= \a -> nextHandleIdx a >> writeAsset 0 Nothing a >> pure a
        it "should write Failed on a Left err" $ do
            let msg = "Failed in test"
            assets <- makeAssets
            handleLoadResult 0 (simpleFailure msg) assets
            assetStatus assets handle `shouldReturn` Failed msg
            getAsset assets handle `shouldReturn` Nothing @Int
        it "should write Loaded on Right with no dependencies" $ do
            assets <- makeAssets
            handleLoadResult 0 (simpleSuccess dynAsset) assets
            assetStatus assets handle `shouldReturn` Loaded
            getAsset assets handle `shouldReturn` Just intAsset
            waiting <- waitingAssets assets
            Seq.length waiting `shouldBe` 0
        it "should not write asset if it has dependencies" $ do
            let loadedAsset = LoadedAsset {asset = dynAsset, dependencies = Vec.fromList [dependency (Handle 1), dependency (Handle 2)]}
            assets <- makeAssets
            handleLoadResult 0 (LoadSuccess loadedAsset) assets
            assetStatus assets handle `shouldReturn` WaitingOnDependencies
            getAsset assets handle `shouldReturn` Nothing @Int
            waiting <- waitingAssets assets
            Seq.length waiting `shouldBe` 1

insertAssetLoaderTest :: Spec
insertAssetLoaderTest =
    describe "insertAssetLoader" $ do
        let simpleLoader      = SimpleAssetLoader @SimpleAsset
            conflictingLoader = ConflictingAssetLoader @ConflictingAsset
        it "should have no loaders for any extensions" $ do
            loaders <- assetLoaders <$> emptyAssets def
            loaders `shouldSatisfyNS` Map.null
        it "should have one associated extension" $ do
            loaders <- assetLoaders . insertAssetLoader simpleLoader <$> emptyAssets def
            Map.size loaders `shouldBe` 1
            loaders `shouldHaveMembersNS` [simpleExtension]
        it "should have two extensions associated for one loader" $ do
            loaders <- assetLoaders . insertAssetLoader conflictingLoader <$> emptyAssets def
            Map.size loaders `shouldBe` 2
            loaders `shouldHaveMembersNS` [simpleExtension, conflictingExtension]
        it "should override the previous loader for extension" $ do
            assets <- insertAssetLoader simpleLoader <$> emptyAssets def
            let assets'  = insertAssetLoader conflictingLoader assets
                loaders  = assetLoaders assets
                loaders' = assetLoaders assets'
            Map.size loaders `shouldBe` 1
            loaders `shouldHaveMembersNS` [simpleExtension]
            Map.size loaders' `shouldBe` 2
            loaders' `shouldHaveMembersNS` [simpleExtension, conflictingExtension]

labeledTest :: Spec
labeledTest =
    describe "labeled" $ do
        let labeled' assets i l v = runReaderT (unContext $ labeled i l v) assets
            intAsset = 12345 :: Int
        it "should write and mark loaded with no dependencies" $ do
            assets <- emptyAssets def
            h      <- labeled' assets "file.ext" "label" $ LoadedAsset {asset = intAsset, dependencies = Vec.empty}
            assetStatus assets h `shouldReturn` Loaded
            getAsset assets h `shouldReturn` Just intAsset
            waiting <- waitingAssets assets
            Seq.length waiting `shouldBe` 0
        it "shouldn't write and mark WaitingOnDependencies with dependencies" $ do
            let loaded    = LoadedAsset {asset = intAsset,       dependencies = Vec.singleton (dependency (Handle 1))}
            assets <- emptyAssets def
            h      <- labeled' assets "file.ext" "label" loaded
            assetStatus assets h `shouldReturn` WaitingOnDependencies
            getAsset assets h `shouldReturn` Nothing
            waiting <- waitingAssets assets
            Seq.length waiting `shouldBe` 1

loadHandleTest :: Spec
loadHandleTest =
    describe "loadHandle" $ do
        let loader = SimpleAssetLoader @SimpleAsset
        it "should insert load request for missing valid asset" $ do
            assets <- insertAssetLoader loader <$> emptyAssets def
            h <- loadHandle @_ @SimpleAsset assets simplePath
            h `shouldBe` Handle 0
            assetStatus assets h `shouldReturn` NotLoaded
            getAsset assets h `shouldReturn` Nothing
            toLoad <- readMVar (assetsToLoad assets)
            case toLoad of
                ((i, _, info) :<| Seq.Empty) -> do
                    i `shouldBe` 0
                    info `shouldBe` AssetInfo simplePath (Handle 0)
                _ -> expectationFailure $ "assetsToLoad should have 1, had " ++ show (Seq.length toLoad)
        it "should be Failed for missing loader" $ do
            assets <- emptyAssets def
            h <- loadHandle @_ @SimpleAsset assets simplePath
            h `shouldBe` Handle 0
            assetStatus assets h `shouldReturn` Failed ("No loader for extension `" ++ T.unpack simpleExtension ++ "`")
            toLoad <- readMVar (assetsToLoad assets)
            toLoad `shouldSatisfyNS` Seq.null
        it "shouldn't insert for existing asset" $ do
            assets <- insertAssetLoader loader <$> emptyAssets def
            h1 <- loadHandle @_ @SimpleAsset assets simplePath
            h2 <- loadHandle @_ @SimpleAsset assets simplePath
            h2 `shouldBe` h1

loadSetSummaryTest :: Spec
loadSetSummaryTest =
    describe "loadSetSummary" $ do
        it "should report loaded for empty load sets" $ do
            assets <- emptyAssets def
            loadSetSummary assets (loadSet []) `shouldReturn` Loaded
        it "should report failure if any elements fail" $ do
            assets <- emptyAssets def
            writeAssetStatus 0 Loaded assets
            writeAssetStatus 1 (Failed "test") assets
            let ls = loadSet [awaitHandle (Handle 0), awaitHandle (Handle 1)]
            loadSetSummary assets ls `shouldReturn` Failed "test"
        it "should report Loaded if all elements are Loaded" $ do
            assets <- emptyAssets def
            writeAssetStatus 0 Loaded assets
            writeAssetStatus 1 Loaded assets
            let ls = loadSet [awaitHandle (Handle 0), awaitHandle (Handle 1)]
            loadSetSummary assets ls `shouldReturn` Loaded
        it "should not be loaded if not all assets are finished" $ do
            assets <- emptyAssets def
            writeAssetStatus 0 Loaded assets
            writeAssetStatus 1 Loading assets
            let ls = loadSet [awaitHandle (Handle 0), awaitHandle (Handle 1)]
            loadSetSummary assets ls `shouldReturn` Loading

updateWaitingTest :: Spec
updateWaitingTest =
    describe "updateWaiting" $ do
        let setup statuses a = do
                assets <- emptyAssets def
                h    <- nextHandleIdx assets
                writeAssetStatus h WaitingOnDependencies assets
                deps <- forM statuses $ \s -> do
                    dh <- nextHandleIdx assets
                    writeAssetStatus dh s assets
                    pure dh
                let la = LoadedAsset {asset = toDyn (12345 :: Int), dependencies = Vec.fromList (map (dependency . Handle) deps)}
                modifyMVar_ (assetsWaiting assets) (pure . (|>(h, Right la)))
                assets `shouldHaveWaiting` Set.fromList [Handle h]
                updateWaiting assets
                a assets (Handle h)
        it "should be waiting when dependencies aren't loaded" $ do
            setup [Loading, Loaded] $ \assets h -> do
                assets `shouldHaveWaiting` Set.fromList [h]
                assetStatus assets h `shouldReturn` WaitingOnDependencies
        it "should be finished when dependencies are successfully loaded" $ do
            setup [Loaded, Loaded] $ \assets h -> do
                assets `shouldHaveWaiting` Set.empty
                assetStatus assets h `shouldReturn` Loaded
        it "should be failed when any dependencies are failed" $ do
            setup [Loaded, Loading, Failed ""] $ \assets h -> do
                assets `shouldHaveWaiting` Set.empty
                assetStatus assets h `shouldReturn` Failed ""

withResourceTest :: Spec
withResourceTest =
    describe "withResource" $ do
        it "shoud be able to fetch given resource" $ do
            let res = 123 :: Int
            assets <- insertLoaderResource res <$> emptyAssets def
            result <- flip runReaderT assets . unContext . withResource $ \(i :: Int) ->
                pure (simpleSuccess i)
            result `shouldBe` simpleSuccess res
        it "should report asset load failure if resource is missing" $ do
            assets <- emptyAssets def
            result <- flip runReaderT assets . unContext . withResource $ \(f :: Float) ->
                pure (simpleSuccess f)
            result `shouldSatisfy` (\case {LoadFailure _ -> True; _ -> False})

-----------
-- Utils --
-----------

waitingAssets :: Assets -> IO (Seq (Int, Either (Handle ()) (LoadedAsset Dynamic)))
waitingAssets Assets{..} = readMVar assetsWaiting 

shouldHaveWaiting :: Assets -> Set (Handle a) -> Expectation
shouldHaveWaiting assets expected = do
    waiting <- waitingAssets assets
    let handles = Set.fromList . map (Handle . fst) . F.toList $ waiting
    handles `shouldBe` expected

simpleExtension, conflictingExtension, complexExtension :: Text
simpleExtension      = "simple.txt"
conflictingExtension = "conflict.txt"
complexExtension     = "complex.txt"

simplePath :: Path
simplePath = "test/example.simple.txt"

newtype SimpleAsset = SimpleAsset Int deriving (Eq, Read, Show)
data SimpleAssetLoader r = SimpleAssetLoader
instance SimpleAssetLoader `CanLoad` SimpleAsset where
    extensions _  = Set.singleton simpleExtension
    loadAsset _ _ = pure . simpleSuccess . read . T.unpack . T.decodeUtf8

newtype ConflictingAsset = ConflictingAsset Bool deriving (Eq, Read, Show)
data ConflictingAssetLoader r = ConflictingAssetLoader
instance ConflictingAssetLoader `CanLoad` ConflictingAsset where
    extensions _  = Set.fromList [simpleExtension, conflictingExtension]
    loadAsset _ _ = pure . simpleSuccess . read . T.unpack . T.decodeUtf8

data ComplexAssetConfig = ComplexAssetConfig
    { simpleAssetPath     :: !Path
    , embeddedSimpleAsset :: !SimpleAsset
    } deriving (Eq, Read, Show)

data ComplexAsset = ComplexAsset
    { simple1 :: Handle SimpleAsset
    , simple2 :: Handle SimpleAsset
    } deriving (Eq, Read, Show)
data ComplexAssetLoader r = ComplexAssetLoader
instance ComplexAssetLoader `CanLoad` ComplexAsset where
    extensions _ = Set.singleton complexExtension
    loadAsset _ AssetInfo{..} bytes = do
        let ComplexAssetConfig{..} = read . T.unpack . T.decodeUtf8 $ bytes
        simple1 <- getHandle $ Path.combine (Path.assetDirectory assetPath) simpleAssetPath
        simple2 <- labeled assetPath "embedded" (simpleLoaded embeddedSimpleAsset)
        pure $ LoadSuccess $ LoadedAsset { asset         = ComplexAsset {..}
                                         , dependencies  = Vec.fromList [dependency simple1, dependency simple2] }

shouldSatisfyNS :: HasCallStack => a -> (a -> Bool) -> Expectation
shouldSatisfyNS v p = unless (p v) $ HUnit.assertFailure msg
    where msg = "predicate failed"

shouldHaveMembersNS :: (HasCallStack, Foldable t, Ord k, Show k) => Map k a -> t k -> Expectation
shouldHaveMembersNS m keys = unless (expectedSet `Set.isSubsetOf` mapKeys) (HUnit.assertFailure msg)
    where msg         = concat ["no members ", show expectedSet, " in keys: ", show mapKeys]
          expectedSet = Set.fromList $ F.toList keys
          mapKeys     = Set.fromList $ Map.keys m