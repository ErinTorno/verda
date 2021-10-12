module Verda.Asset.Internal where

import           Control.Concurrent
import           Control.Exception.Base  (SomeException, catch)
import           Control.Monad.Reader
import           Control.Monad.ST        (stToIO)
import           Data.ByteString         (ByteString)
import           Data.Default
import           Data.Dynamic
import qualified Data.Foldable           as F
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.Map.Strict         as Map
import           Data.Proxy
import           Data.Sequence           ((|>))
import qualified Data.Sequence           as Seq
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as Vec
import qualified Data.Vector.Mutable     as MVec
import           Say
import           Type.Reflection

import qualified Verda.Asset.Path        as Path
import           Verda.Asset.Types
import           Verda.Util.Container    (growIfNeeded)
import           Verda.Util.Error        (sayErrAndExit)

------------
-- Assets --
------------

emptyAssets :: MonadIO m => AssetSettings -> m Assets
emptyAssets settings = liftIO $ Assets settings Map.empty Map.empty
    <$> newMVar 0
    <*> (newMVar =<< MVec.new len)
    <*> (newMVar =<< MVec.new len)
    <*> (newMVar =<< stToIO (HT.newSized len))
    <*> newMVar Seq.empty
    <*> newMVar Seq.empty
    <*> newMVar Seq.empty
    where len = defaultAssetLen settings

-- Config --

-- | Associates a new AssetLoader with the Assets
-- | AssetLoaders with conflicting extensions will favor the most recently added AssetLoader
insertAssetLoader :: (a `CanLoad` r, Typeable a, Typeable r) => a r -> Assets -> Assets
insertAssetLoader loader assets@Assets{..} = assets {assetLoaders = Map.union newMap assetLoaders }
    where exts        = extensions proxy
          newMap      = Map.fromSet (const (Loader (someTypeRep proxy) (isSingleThreadOnly proxy) loadfn)) exts
          loadfn      = mkDyn proxy (loadAsset proxy)
          mkDyn :: Typeable r => Proxy (a r) -> AssetLoadFn r -> AssetLoadFn Dynamic
          mkDyn _ f = let d (HandleAlias h)   = HandleAlias $ coerceHandle h
                          d (LoadSuccess a)   = LoadSuccess $ fmap toDyn a
                          d (LoadFailure err) = LoadFailure err
                       in \info bytes -> d <$> f (coerceAssetInfo info) bytes
          proxy     = mkProxy loader
          mkProxy :: a r -> Proxy (a r)
          mkProxy _ = Proxy

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
    nextID <- modifyMVar nextHandleID $ \i -> pure (i + 1, i)
    -- grow the vectors if necessary
    takeMVar assetStatuses >>= growIfNeeded nextID 16 >>= putMVar assetStatuses
    takeMVar loadedAssets  >>= growIfNeeded nextID 16 >>= putMVar loadedAssets
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
handleLoadResult idx (LoadFailure msg) assets =
    writeAssetStatus idx (Failed msg) assets
handleLoadResult idx (LoadSuccess lr@LoadedAsset{..}) assets@Assets{..} = liftIO $ do
    status <- if Vec.null dependencies
              then do
                  loaded <- readMVar loadedAssets
                  MVec.write loaded idx (Just asset)
                  pure Loaded
              else do
                  modifyMVar_ assetsWaiting (pure . (|>(idx, Right lr)))
                  pure WaitingOnDependencies
    writeAssetStatus idx status assets
handleLoadResult idx (HandleAlias handle@(Handle h)) assets@Assets{..} = liftIO $ do
    loadedVec <- readMVar loadedAssets
    statusVec <- readMVar assetStatuses
    status <- MVec.read statusVec h
    writeAssetStatus idx status assets
    if Loaded == status
    then do
        value  <- MVec.read loadedVec h
        MVec.write loadedVec idx value
    else
        modifyMVar_ assetsWaiting (pure . (|>(idx, Left (coerceHandle handle))))

-- | Run checks to determine if asset's dependencies are finished loaded
updateWaiting :: MonadIO m => Assets -> m ()
updateWaiting assets@Assets{..} = liftIO $ do
    waiting <- modifyMVar assetsWaiting (pure . (Seq.empty,))
    forM_ waiting $ \(idx, r) -> case r of 
        Left parHandle@(Handle parIdx) -> do
            status <- assetStatus assets parHandle
            if isFinished status
            then do
                loaded <- takeMVar loadedAssets
                MVec.read loaded parIdx >>= \case
                    Just val -> do
                        writeAsset idx (Just val) assets
                    Nothing -> pure ()
                writeAssetStatus idx status assets
            else
                modifyMVar_ assetsWaiting (pure . (|>(idx, Left parHandle)))
        Right la@LoadedAsset{..} -> do
            maxStatus <- foldM (\s (Depedency h) -> mostSevere s <$> assetStatus assets h) Loaded dependencies
            if isFinished maxStatus
            then do
                writeAssetStatus idx maxStatus assets
                writeAsset idx (Just asset) assets
            else
                modifyMVar_ assetsWaiting (pure . (|>(idx, Right la)))

forkAssetLoader :: MonadIO m => Assets -> m ThreadId
forkAssetLoader assets@Assets{..} = liftIO . forkIO . forever $ do
    toLoad <- modifyMVar assetsToLoad (pure . (Seq.empty,))
    -- Run loaders
    let proc idx load info@AssetInfo{..} = void . forkIO $ do
            bytes  <- readAssetFileBytes assets assetPath
            result <- runReaderT (unContext $ load (coerceAssetInfo info) bytes) assets
            handleLoadResult idx result assets
        markFail idx (e :: SomeException) = writeAssetStatus idx (Failed $ show e) assets
    forM_ toLoad $ \(idx, load, info) ->
        proc idx load info `catch` markFail idx
    updateWaiting assets
    liftIO $ threadDelay (threadDelayMS assetSettings)

updateSingleThreaded :: MonadIO m => Assets -> m ()
updateSingleThreaded assets@Assets{..} = liftIO $ do
    toLoad <- modifyMVar assetsToLoadSingThreaded (pure . (Seq.empty,))
    forM_ toLoad $ \(idx, load, info@AssetInfo{..}) ->
        let proc = do
                bytes  <- readAssetFileBytes assets assetPath
                result <- runReaderT (unContext $ load (coerceAssetInfo info) bytes) assets
                handleLoadResult idx result assets
         in proc `catch` (\(e :: SomeException) -> writeAssetStatus idx (Failed $ show e) assets)

readAssetFileBytes :: Assets -> Path -> IO ByteString
readAssetFileBytes Assets{..} = readAssetFile assetSettings . unPath . Path.addAssetDir (assetFolder assetSettings)

coerceAssetInfo :: AssetInfo a -> AssetInfo b
coerceAssetInfo info@AssetInfo{..} = info {assetHandle = coerceHandle assetHandle}

---------------
-- Resources --
---------------

withResource :: Typeable a => (a -> LoadContext (LoadResult b)) -> LoadContext (LoadResult b)
withResource action = withResourceMaybe $ \key -> \case
    Just r  -> action r
    Nothing -> pure . simpleFailure $ "Required missing asset resource of type " ++ show key
    
withResourceOrDef :: (Default a, Typeable a) => (a -> LoadContext (LoadResult b)) -> LoadContext (LoadResult b)
withResourceOrDef action = withResourceMaybe $ \_ -> \case
    Just r  -> action r
    Nothing -> action def

withResourceMaybe :: Typeable a => (SomeTypeRep -> Maybe a -> LoadContext (LoadResult b)) -> LoadContext (LoadResult b)
withResourceMaybe action = do
    let mkProxy :: (SomeTypeRep -> Maybe a -> LoadContext (LoadResult b)) -> Proxy a
        mkProxy = const Proxy
        proxy   = mkProxy action
        key     = someTypeRep proxy
        unDyn :: Typeable a => Dynamic -> LoadContext a
        unDyn r@(Dynamic rep _) = case fromDynamic r of
            Just res -> pure res
            Nothing  -> sayErrAndExit $ T.concat [ "LoadResource was assigned to a key of the wrong type (was "
                                                 , T.pack $ show rep
                                                 , " but assigned as "
                                                 , T.pack $ show key
                                                 , "); This is a bug and should never happen"
                                                 ]
    Map.lookup key <$> asks loaderResources >>= \case
        Just res -> unDyn res >>= action key . Just
        Nothing  -> action key Nothing

----------------
-- LoadResult --
----------------

dependency :: Handle a -> Dependency
dependency = Depedency . coerceHandle

simpleLoaded :: a -> LoadedAsset a
simpleLoaded val = LoadedAsset val Vec.empty

simpleSuccess :: a -> LoadResult a
simpleSuccess = LoadSuccess . simpleLoaded

simpleFailure :: String -> LoadResult a
simpleFailure = LoadFailure

simpleAlias :: Handle a -> LoadResult a
simpleAlias = HandleAlias

simpleEither :: Either String a -> LoadResult a
simpleEither (Left err)  = simpleFailure err
simpleEither (Right val) = simpleSuccess val

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
labeled :: Typeable a => Path -> Text -> LoadedAsset a -> LoadContext (Handle a)
labeled path label la@LoadedAsset{..} = do
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
            modifyMVar_ (assetsWaiting assets) (pure . (|>(nextID, Right $ la {asset = toDyn asset})))
        pure (Handle nextID)

loadHandle :: MonadIO m => Assets -> Path -> m (Handle a)
loadHandle assets path = liftIO $ runReaderT (unContext $ getHandle path) assets

getHandle :: Path -> LoadContext (Handle a)
getHandle path = getHandleWith path (\_ -> pure ())

getHandleWith :: Path -> (Handle a -> LoadContext ()) -> LoadContext (Handle a)
getHandleWith path action = do
    handlesMVar <- asks handlesByPath
    handles     <- liftIO $ readMVar handlesMVar
    (liftIO . stToIO $ HT.lookup handles path) >>= \case
        Just (Handle i) -> pure $ Handle i
        Nothing         -> do
            let ext       = Path.assetExtension path
            assets     <- ask
            loaderMap  <- asks assetLoaders
            toLoadMultRef <- asks assetsToLoad
            toLoadSingRef <- asks assetsToLoadSingThreaded
            nextID <- liftIO $ do 
                nextID <- nextHandleIdx assets
                assocPathWithHandle path (Handle nextID) assets
                pure nextID
            action $ Handle nextID
            liftIO $ do
                status <- case Map.lookup ext loaderMap of
                    Nothing -> do
                        let errMsg = "No loader for extension `" <> ext <> "`"
                        sayErr errMsg
                        pure $ Failed $ T.unpack errMsg
                    Just Loader{..} -> do
                        let loadRow = (nextID, loadFn, AssetInfo path $ Handle nextID)
                        modifyMVar_ (if isSingleThreaded then toLoadSingRef else toLoadMultRef) (pure . (|>loadRow))
                        pure NotLoaded
                writeAsset nextID Nothing assets
                writeAssetStatus nextID status assets
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