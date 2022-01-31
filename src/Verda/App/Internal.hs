module Verda.App.Internal where

import           Apecs                        hiding (Map)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Managed
import           Data.Default
import           Data.Functor
import           Data.IORef
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Text                    (Text)
import           Data.Typeable
import qualified SDL

import           Verda.App.System
import           Verda.Asset
import qualified Verda.Asset.Internal
import           Verda.Event.Control.Internal (mkControlState)
import           Verda.Event.Handler          (handleEvents)
import           Verda.Graphics.Components
import           Verda.Graphics.Texture
import qualified Verda.Graphics.Vulkan.Types  as Vulkan
import qualified Verda.Graphics.Vulkan.Window as Vulkan
import qualified Verda.System.Window          as Window
import           Verda.Util.Logger
import           Verda.World

data App w s = App
    { appTitle             :: !Text
    , appWindowConfig      :: !SDL.WindowConfig
    , appIcon              :: !(Maybe Path)
    , appWorld             :: !(IO w)
    , appStartups          :: !(Map (StateLifetime s) [StateStartup w s])
    , appSystems           :: !(Map (StateLifetime s) [StateSystem w s])
    , appFinalizers        :: !(Map (StateLifetime s) [StateFinalizer w s])
    , appInitStateID       :: !s
    , appAssets            :: !Assets
    , appTargetRefreshRate :: !(Maybe Float)
    , appTargetTickRate    :: !(Maybe Float) -- TODO implement
    , appLogger            :: !Logger
    }

-- | Makes an app with the given World initializer
makeApp :: MonadIO m => IO w -> m (App w ())
makeApp = makeAppWith def

-- | Makes an app with the given asset loading settings and world initializer
makeAppWith :: MonadIO m => AssetSettings -> IO w -> m (App w ())
makeAppWith settings mkWorld = do
    texQualOverrides <- mkTextureScaleQualityOverrides
    assets <- emptyAssets settings
          <&> insertLoaderResource ScaleLinear
            . insertLoaderResource texQualOverrides
            . insertAssetLoader (TextureLoader @Texture)
            . insertAssetLoader (TextureLoader @Icon)
    let conf = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.VulkanContext
                                 , SDL.windowInitialSize = fromIntegral <$> unWindowResolution def}
    pure $ App "Untitled App" conf Nothing mkWorld Map.empty Map.empty Map.empty () assets Nothing (Just 120) def

-- Setup --

-- | Updates the SDL WindowConfig for the app
updateWindowConfig :: (SDL.WindowConfig -> SDL.WindowConfig) -> App w s -> App w s
updateWindowConfig f app = app {appWindowConfig = f (appWindowConfig app)}

-- | Sets the title of the app
withTitle :: Text -> App w s -> App w s
withTitle title app = app {appTitle = title}

-- | Adds a type of asset loader a for loading r
withAssetLoader :: (a `CanLoad` r, Typeable r) => a r -> App w s -> App w s
withAssetLoader loader app = app {appAssets = insertAssetLoader loader $ appAssets app}

-- | Adds a loader resource for assets to access while loading
withLoaderResource :: Typeable a => a -> App w s -> App w s
withLoaderResource val app = app {appAssets = insertLoaderResource val $ appAssets app}

-- | Sets the initial state to begin with during app startup
-- Warning: this will reset startups, systems, and finalizers for the previous State type
withInitState :: s -> App w o -> App w s
withInitState st app = app {appInitStateID = st, appStartups = Map.empty, appSystems = Map.empty, appFinalizers = Map.empty}

-- | Adds a startup action for the given state
withStartup :: Ord s => s -> StateStartup w s -> App w s -> App w s
withStartup = withStartupForLifetime . ForState

-- | Adds a startup action for all states
withStartupForAll :: Ord s => StateStartup w s -> App w s -> App w s
withStartupForAll = withStartupForLifetime ForAnyState

-- | Adds a startup action for the given lifetime
withStartupForLifetime :: Ord s => StateLifetime s -> StateStartup w s -> App w s -> App w s
withStartupForLifetime lifetime sys app = app {appStartups = Map.alter addStartup lifetime (appStartups app)}
    where addStartup (Just l) = Just $ sys:l
          addStartup Nothing  = Just [sys]

-- | Adds a continually-run action for the given state
withSystem :: Ord s => s -> StateSystem w s -> App w s -> App w s
withSystem = withSystemForLifetime . ForState

-- | Adds a continually-run action for all states
withSystemForAll :: Ord s => StateSystem w s -> App w s -> App w s
withSystemForAll = withSystemForLifetime ForAnyState

-- | Adds a continually-run action for the given lifetime
withSystemForLifetime :: Ord s => StateLifetime s -> StateSystem w s -> App w s -> App w s
withSystemForLifetime lifetime sys app = app {appSystems = Map.alter addSystem lifetime (appSystems app)}
    where addSystem (Just l) = Just $ sys:l
          addSystem Nothing  = Just [sys]
          
-- | Adds a finishing action for the given lifetime
withFinalizer :: Ord s => s -> StateFinalizer w s -> App w s -> App w s
withFinalizer = withFinalizerForLifetime . ForState

-- | Adds a finishing action for all states
withFinalizerForAll :: Ord s => StateFinalizer w s -> App w s -> App w s
withFinalizerForAll = withFinalizerForLifetime ForAnyState

-- | Adds a finishing action for the given state
withFinalizerForLifetime :: Ord s => StateLifetime s -> StateFinalizer w s -> App w s -> App w s
withFinalizerForLifetime lifetime sys app = app {appFinalizers = Map.alter addFinalizer lifetime (appFinalizers app)}
    where addFinalizer (Just l) = Just $ sys:l
          addFinalizer Nothing  = Just [sys]

-- | Sets the target number of render refreshers per second for the app (default is Just 120, Nothing is no cap)
withTargetRefreshRate :: Maybe Float -> App w s -> App w s
withTargetRefreshRate rate app = app {appTargetRefreshRate = rate}

-- | Sets the Logger that the app will use
withLogger :: Logger -> App w s -> App w s
withLogger logger app = app {appLogger = logger}

withIcon :: Path -> App w s -> App w s
withIcon iconPath app = app {appIcon = Just iconPath}

withDefaultSystems :: (Ord s, VerdaWorld w IO) => App w s -> App w s
withDefaultSystems = withSystemForAll Window.iconSystem

-- Running --

-- | Starts the given app and runs its systems and asset loaders until the ShouldQuit global is True
start :: Ord s => (VerdaWorld w IO) => App w s -> IO ()
start app@App{..} = runManaged $ Vulkan.run (Vulkan.WindowCreateInfo {Vulkan.sdlWindowConfig = appWindowConfig, Vulkan.logger = def,..}) $ \windowIORef render -> do
    window@Vulkan.VulkanWindow{..} <- readIORef windowIORef
    targetRefreshRate <- case appTargetRefreshRate of
        Just r  -> pure r
        Nothing -> Vulkan.getDisplayModeRefreshRate window def
    res   <- SDL.get $ SDL.windowSize vwSDLWindow
    let finAssets = insertLoaderResource vwSDLRenderer appAssets
    void $ Verda.Asset.Internal.forkAssetLoader finAssets
    world <- appWorld
    time  <- SDL.time
    runWith world $ do
        global $= appAssets
        global $= appLogger
        global $= WindowResolution (fromIntegral <$> res)
        global $= Time 0 time
        global $= TargetRefreshRate targetRefreshRate
        global $= Window window
        forM_ appIcon $ \iconPath -> do
            assets <- get global
            hIcon  <- loadHandle assets iconPath
            global $= LoadIcon (Just hIcon)
        set global =<< mkControlState
        let loop !s !startups !systems !lastRenderTime !lastTime = do
                window' <- liftIO $ readIORef windowIORef
                global  $= Window window'
                res'    <- SDL.get $ SDL.windowSize (Vulkan.vwSDLWindow window')
                global  $= WindowResolution (fromIntegral <$> res')
                handleEvents
                newTime <- SDL.time
                Verda.Asset.Internal.updateSingleThreaded finAssets
                global  $= Time (newTime - lastTime) newTime
                (s', startups', systems') <- runSystems app s startups systems
                TargetRefreshRate refreshRate <- get global
                newRenderTime <- runIfReady refreshRate lastRenderTime (liftIO $ runManaged render)
                quit    <- shouldQuit <$> get global
                unless quit $
                    loop s' startups' systems' newRenderTime newTime
        loop appInitStateID (startupsFor app appInitStateID) (systemsFor app appInitStateID) 0 =<< SDL.time

runSystems :: Ord s => App w s -> s -> [StateStartup w s] -> [StateSystem w s] -> SystemT w IO (s, [StateStartup w s], [StateSystem w s])
runSystems app !stID startups systems = do
    let run s []         = pure (s, [], systems)
        run s (sys:rest) = sys s >>= \s' ->
            if   s == s'
            then run s rest
            else do
                mapM_ ($s') (finalizersFor app s)
                pure (s', startupsFor app s', systemsFor app s')
    (s', startups', systems') <- filterM (fmap (==Again) . ($stID)) startups >>= \case -- fmap (==Again)
        []        -> run stID systems
        startups' -> pure (stID, startups', systems)
    pure (s', startups', systems')

startupsFor ::  Ord s => App w s -> s -> [StateStartup w s]
startupsFor App{..} !stID = fromMaybe [] (Map.lookup ForAnyState appStartups) ++ fromMaybe [] (Map.lookup (ForState stID) appStartups)

systemsFor ::  Ord s => App w s -> s -> [StateSystem w s]
systemsFor App{..} !stID = fromMaybe [] (Map.lookup ForAnyState appSystems) ++ fromMaybe [] (Map.lookup (ForState stID) appSystems)

finalizersFor ::  Ord s => App w s -> s -> [StateFinalizer w s]
finalizersFor App{..} !stID = fromMaybe [] (Map.lookup ForAnyState appFinalizers) ++ fromMaybe [] (Map.lookup (ForState stID) appFinalizers)

runIfReady :: Float -> Float -> SystemT w IO () -> SystemT w IO Float
runIfReady !cappingRatePerSec !lastTime action
    | cappingRatePerSec <= 0 = SDL.time
    | otherwise = do
        let delay = 1 / cappingRatePerSec
        startTime <- SDL.time
        if startTime - lastTime >= delay
        then action >> SDL.time
        else pure lastTime