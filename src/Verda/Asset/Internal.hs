module Verda.Asset.Internal where

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.ST        (stToIO)
import           Data.ByteString         (ByteString)
import           Data.Dynamic
import qualified Data.Foldable           as F
import qualified Data.HashTable.ST.Basic as HT
import           Data.IORef
import qualified Data.Map.Strict         as Map
import           Data.Proxy
import           Data.Sequence           ((|>))
import qualified Data.Sequence           as Seq
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as Vec
import qualified Data.Vector.Mutable     as MVec
import           Type.Reflection

import qualified Verda.Asset.Path        as Path
import           Verda.Asset.Types
import           Verda.Util.Container    (growIfNeeded)

------------
-- Assets --
------------

emptyAssets :: MonadIO m => AssetSettings -> m Assets
emptyAssets settings = liftIO $ Assets settings Map.empty Map.empty
    <$> newIORef 0
    <*> (newMVar =<< MVec.new len)
    <*> (newMVar =<< MVec.new len)
    <*> (newMVar =<< stToIO (HT.newSized len))
    <*> newIORef Seq.empty
    <*> newIORef Seq.empty
    where len = defaultAssetLen settings

-- Config --

-- | Associates a new AssetLoader with the Assets
-- | AssetLoaders with conflicting extensions will favor the most recently added AssetLoader
insertAssetLoader :: (a `CanLoad` r, Typeable r) => a r -> Assets -> Assets
insertAssetLoader loader assets@Assets{..} = assets {assetLoaders = Map.union newMap assetLoaders }
    where exts        = extensions proxy
          newMap      = Map.fromSet (const loadfn) exts
          loadfn info = fmap (fmap mkDynLoad) . convince loader (loadAsset proxy) info
          mkDynLoad LoadedAsset{..} = LoadedAsset {asset = toDyn asset,..}
          proxy     = mkProxy loader
          mkProxy :: a r -> Proxy (a r)
          mkProxy _ = Proxy
          convince :: a r -> AssetLoadFn r -> AssetLoadFn r
          convince _ = id

insertLoaderResource :: Typeable a => a -> Assets -> Assets
insertLoaderResource val assets@Assets{..} = assets {loaderResources = Map.insert key (toDyn val) loaderResources}
    where key     = someTypeRep $ mkProxy val
          mkProxy :: a -> Proxy a
          mkProxy = const Proxy

-- Internal --

assocPathWithHandle :: MonadIO m => Path -> Handle a -> Assets -> m ()
assocPathWithHandle path handle Assets{..} = liftIO $ do
    handlesMap <- takeMVar handlesByPath
    stToIO $ HT.insert handlesMap path (coerceHandle handle)
    putMVar handlesByPath handlesMap

nextHandleIdx :: MonadIO m => Assets -> m Int
nextHandleIdx Assets{..} = liftIO $ do
    nextID <- atomicModifyIORef' nextHandleID $ \i -> (i + 1, i)
    -- grow the vectors if necessary
    takeMVar assetStatuses >>= growIfNeeded nextID 16 >>= putMVar assetStatuses
    takeMVar loadedAssets >>= growIfNeeded nextID 16 >>= putMVar loadedAssets
    pure nextID

writeAsset :: MonadIO m => Int -> Maybe Dynamic -> Assets -> m ()
writeAsset idx asset Assets{..} = liftIO $ do
    loaded <- takeMVar loadedAssets
    MVec.write loaded idx asset
    putMVar loadedAssets loaded

writeAssetStatus :: MonadIO m => Int -> AssetStatus -> Assets -> m ()
writeAssetStatus idx status Assets{..} = liftIO $ do
    statuses <- takeMVar assetStatuses
    MVec.write statuses idx status
    putMVar assetStatuses statuses

handleLoadResult :: MonadIO m => Int -> LoadResult Dynamic -> Assets -> m ()
handleLoadResult idx (Left msg) assets = writeAssetStatus idx (Failed msg) assets
handleLoadResult idx (Right lr@LoadedAsset{..}) assets@Assets{..} = liftIO $ do
    status <- if Vec.null dependencies
              then do
                  loaded <- readMVar loadedAssets
                  MVec.write loaded idx (Just asset)
                  pure Loaded
              else do
                  atomicModifyIORef' assetsWaiting ((,()) . (|>(idx, lr)))
                  pure WaitingOnDependencies
    writeAssetStatus idx status assets

-- | Run checks to determine if asset's dependencies are finished loaded
updateWaiting :: MonadIO m => Assets -> m ()
updateWaiting assets@Assets{..} = liftIO $ do
    waiting <- atomicModifyIORef' assetsWaiting (Seq.empty,)
    forM_ waiting $ \(idx, la@LoadedAsset{..}) -> do
        maxStatus <- foldM (\s (Depedency h) -> mostSevere s <$> assetStatus assets h) Loaded dependencies
        if isFinished maxStatus
        then do
            writeAssetStatus idx maxStatus assets
            writeAsset idx (Just asset) assets
        else
            atomicModifyIORef' assetsWaiting $ (,()) . (|>(idx, la))

forkAssetLoader :: MonadIO m => Assets -> m ThreadId
forkAssetLoader assets@Assets{..} = liftIO . forkIO . forever $ do
    toLoad <- atomicModifyIORef' assetsToLoad (Seq.empty,)
    -- Run loaders
    forM_ toLoad $ \(idx, load, info@(AssetInfo path)) -> void . forkIO $ do
        bytes  <- readAssetFileBytes assets path
        result <- runReaderT (unContext $ load info bytes) assets
        handleLoadResult idx result assets
    updateWaiting assets
    liftIO $ threadDelay (threadDelayMS assetSettings)

readAssetFileBytes :: Assets -> Path -> IO ByteString
readAssetFileBytes Assets{..} = readAssetFile assetSettings . unPath . Path.addAssetDir (assetFolder assetSettings)

-- Resources --

withResource :: Typeable a => (a -> LoadContext (LoadResult b)) -> LoadContext (LoadResult b)
withResource action = do
    let mkProxy :: (a -> LoadContext (LoadResult b)) -> Proxy a
        mkProxy = const Proxy
        proxy   = mkProxy action
        key     = someTypeRep proxy
        err (Dynamic rep _) = error $ concat [ "LoadResource was assigned to a key of the wrong type (was "
                                             , show rep
                                             , " but assigned as "
                                             , show key
                                             , "); This is a bug and should never happen"
                                             ]
    res <- asks loaderResources
    case Map.lookup key res of
        Just r  -> action $ fromDyn r (err r)
        Nothing -> pure . simpleFailure $ "Required missing asset resource of type " ++ show key

----------------
-- LoadResult --
----------------

dependency :: Handle a -> Dependency
dependency = Depedency . coerceHandle

simpleLoaded :: a -> LoadedAsset a
simpleLoaded val = LoadedAsset val Vec.empty

simpleSuccess :: a -> LoadResult a
simpleSuccess = Right . simpleLoaded

simpleFailure :: String -> LoadResult a
simpleFailure = Left

-------------
-- Handles --
-------------

coerceHandle :: Handle a -> Handle b
coerceHandle (Handle i) = Handle i

-- | A predicate that is true if the Handle is successfully loaded
isValid :: (MonadIO m, Typeable a) => Assets -> Handle a -> m Bool
isValid assets h = getAsset assets h >>= \case
    Just _  -> pure True
    Nothing -> pure False

-- | Fetches the asset associated with the handle if it is loaded
getAsset :: (MonadIO m, Typeable a) => Assets -> Handle a -> m (Maybe a)
getAsset Assets{..} (Handle idx) = liftIO $ do
    loaded <- readMVar loadedAssets
    asset  <- MVec.read loaded idx
    pure (asset >>= fromDynamic)

-- | Returns the status of the asset associated with the handle
assetStatus :: MonadIO m => Assets -> Handle a -> m AssetStatus
assetStatus Assets{..} (Handle i) = do
    statuses   <- liftIO $ readMVar assetStatuses
    if   MVec.length statuses < i
    then pure NotLoaded
    else liftIO $ MVec.read statuses i

-- | Associates the given asset with the parent path and a label, and returns a Handle to that asset
-- > labeled (AssetInfo "dir/file.ext") "sub" (LoadedAsset 123 Data.Vector.empty)
labeled :: Typeable a => AssetInfo -> Text -> LoadedAsset a -> LoadContext (Handle a)
labeled (AssetInfo path) label la@LoadedAsset{..} = do
    assets <- ask
    liftIO $ do
        nextID <- nextHandleIdx assets
        assocPathWithHandle (Path.addLabel label path) (Handle nextID) assets
        if Vec.null dependencies
        then do
            writeAsset       nextID (Just $ toDyn asset) assets
            writeAssetStatus nextID Loaded assets
        else do
            writeAssetStatus nextID WaitingOnDependencies assets
            writeAsset nextID Nothing assets
            atomicModifyIORef' (assetsWaiting assets) $ (,()) . (|>(nextID, la {asset = toDyn asset}))
        pure (Handle nextID)

loadHandle :: MonadIO m => Assets -> Path -> m (Handle a)
loadHandle assets path = liftIO $ runReaderT (unContext $ getHandle path) assets

getHandle :: Path -> LoadContext (Handle a)
getHandle path = do
    handlesMVar <- asks handlesByPath
    handles     <- liftIO $ readMVar handlesMVar
    handle      <- liftIO . stToIO $ HT.lookup handles path
    case handle of
        Just (Handle i) -> pure $ Handle i
        Nothing         -> do
            let ext       = Path.assetExtension path
            assets     <- ask
            loaderMap  <- asks assetLoaders
            toLoadRef  <- asks assetsToLoad
            nextID <- liftIO $ do
                nextID <- nextHandleIdx assets
                assocPathWithHandle path (Handle nextID) assets
                status <- case Map.lookup ext loaderMap of
                    Nothing -> pure $ Failed $ "No loader for extension `" ++ T.unpack ext ++ "`"
                    Just loader -> do
                        _ :: () <- atomicModifyIORef' toLoadRef ((,()) . (|>(nextID, loader, AssetInfo path)))
                        pure NotLoaded
                writeAsset nextID Nothing assets
                writeAssetStatus nextID status assets
                pure nextID
            pure $ Handle nextID

------------------
-- AssetLoadSet --
------------------

-- usage: let ls = loadSet [awaitHandle h1, awaitHandle h2, awaitHandle h3]
--        when (isFinished (loadSetSummary assets ls)) $ do

loadSet :: Foldable t => t LoadSetElement -> AssetLoadSet
loadSet = AssetLoadSet . F.toList . F.foldr' Set.insert Set.empty

awaitHandle :: Handle a -> LoadSetElement
awaitHandle = LoadSetElement . coerceHandle

loadSetSummary :: MonadIO m => Assets -> AssetLoadSet -> m AssetStatus
loadSetSummary assets = fmap (F.foldl' combineStatus Loaded) . mapM (assetStatus assets . unLoadSetElement) . unAssetLoadSet
    where combineStatus acc s = case s of
              Failed msg -> case acc of
                  Failed prevMsg -> Failed $ concat [prevMsg, "; ", msg]
                  _              -> Failed msg
              _          -> mostSevere acc s