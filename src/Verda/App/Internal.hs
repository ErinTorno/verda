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

type StateStartup w s = SysContext s -> SystemT w IO StartupResult

type StateSystem w s = SysContext s -> SystemT w IO (SysResult s)

type StateFinalizer w s = SysContext s -> SystemT w IO ()

data App w s = App
    { appTitle             :: !Text
    , appWindowConfig      :: !SDL.WindowConfig
    , appIcon              :: !(Maybe Path)
    , appWorld             :: !(IO w)
    , appStartups          :: !(Map (StateLifetime s) [StateStartup w s])
    , appSystems           :: !(Map (StateLifetime s) [StateSystem w s])
    , appFinalizers        :: !(Map (StateLifetime s) [StateFinalizer w s])
    , appInitStateID       :: !s
    , appInitAssets        :: !Assets
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
    assets <- emptyAssets settings
          <&> insertBundle textureBundle
    let conf = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.VulkanContext
                                 , SDL.windowInitialSize = fromIntegral <$> unWindowResolution def }
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
withAssetLoader loader app = app {appInitAssets = insertAssetLoader loader $ appInitAssets app}

-- | Adds a loader resource for assets to access while loading
withLoaderResource :: Typeable a => a -> App w s -> App w s
withLoaderResource val app = app {appInitAssets = insertLoaderResource val $ appInitAssets app}

-- | Adds a loader resource for assets to access while loading
withLoaderResourceM :: Typeable a => IO a -> App w s -> App w s
withLoaderResourceM val app = app {appInitAssets = insertLoaderResourceM val $ appInitAssets app}

-- | Adds a type of asset loader a for loading r
withBundle :: LoaderBundle a => proxy a -> App w s -> App w s
withBundle bundle app = app {appInitAssets = insertBundle bundle $ appInitAssets app}

-- | Sets the initial state to begin with during app startup
-- Warning: this will reset startups, systems, and finalizers for the previous State type
withInitState :: s -> App w o -> App w s
withInitState st app = app {appInitStateID = st, appStartups = Map.empty, appSystems = Map.empty, appFinalizers = Map.empty}

-- | Adds a startup action for the given state
withStartup :: (IsSystem a w s StartupResult, Ord s) => s -> a -> App w s -> App w s
withStartup = withStartupForLifetime . ForState

-- | Adds a startup action for all states
withStartupForAll :: (IsSystem a w s StartupResult, Ord s) => a -> App w s -> App w s
withStartupForAll = withStartupForLifetime ForAnyState

-- | Adds a startup action that occurs on program start
withBackgroundStartup :: (IsSystem a w s StartupResult, Ord s) => a -> App w s -> App w s
withBackgroundStartup = withStartupForLifetime InBackground

-- | Adds a startup action for the given lifetime
withStartupForLifetime :: (IsSystem a w s StartupResult, Ord s) => StateLifetime s -> a -> App w s -> App w s
withStartupForLifetime lifetime sys app = app {appStartups = Map.alter addStartup lifetime (appStartups app)}
    where addStartup (Just l) = Just $ asStateSystem sys:l
          addStartup Nothing  = Just [asStateSystem sys]

-- | Adds a continually-run action for the given state
withSystem :: (IsSystem a w s (SysResult s), Ord s) => s -> a -> App w s -> App w s
withSystem = withSystemForLifetime . ForState

-- | Adds a continually-run action for all states
withSystemForAll :: (IsSystem a w s (SysResult s), Ord s) => a -> App w s -> App w s
withSystemForAll = withSystemForLifetime ForAnyState

-- | Adds a continually-run action that is not affected by state changes
withBackgroundSystem :: (IsSystem a w s (SysResult s), Ord s) => a -> App w s -> App w s
withBackgroundSystem = withSystemForLifetime InBackground

-- | Adds a continually-run action for the given lifetime
withSystemForLifetime :: (IsSystem a w s (SysResult s), Ord s) => StateLifetime s -> a -> App w s -> App w s
withSystemForLifetime lifetime sys app = app {appSystems = Map.alter addSystem lifetime (appSystems app)}
    where addSystem (Just l) = Just $ asStateSystem sys:l
          addSystem Nothing  = Just [asStateSystem sys]
          
-- | Adds a finishing action for the given lifetime
withFinalizer :: (IsSystem a w s (), Ord s) => s -> a -> App w s -> App w s
withFinalizer = withFinalizerForLifetime . ForState

-- | Adds a finishing action for all states
withFinalizerForAll :: (IsSystem a w s (), Ord s) => a -> App w s -> App w s
withFinalizerForAll = withFinalizerForLifetime ForAnyState

-- | Adds a finishing action that runs at the very end of the program
withBackgroundFinalizer :: (IsSystem a w s (), Ord s) => a -> App w s -> App w s
withBackgroundFinalizer = withFinalizerForLifetime InBackground

-- | Adds a finishing action for the given state
withFinalizerForLifetime :: (IsSystem a w s (), Ord s) => StateLifetime s -> a -> App w s -> App w s
withFinalizerForLifetime lifetime sys app = app {appFinalizers = Map.alter addFinalizer lifetime (appFinalizers app)}
    where addFinalizer (Just l) = Just $ asStateSystem sys:l
          addFinalizer Nothing  = Just [asStateSystem sys]

-- | Sets the target number of render refreshers per second for the app (default is Just 120, Nothing is no cap)
withTargetRefreshRate :: Maybe Float -> App w s -> App w s
withTargetRefreshRate rate app = app {appTargetRefreshRate = rate}

-- | Sets the Logger that the app will use
withLogger :: Logger -> App w s -> App w s
withLogger logger app = app {appLogger = logger}

withIcon :: Path -> App w s -> App w s
withIcon iconPath app = app {appIcon = Just iconPath}

withDefaultSystems :: (Ord s, VerdaWorld w IO) => App w s -> App w s
withDefaultSystems = withBackgroundSystem Window.iconSystem

-- Running --

-- | Starts the given app and runs its systems and asset loaders until the ShouldQuit global is True
start :: Ord s => (VerdaWorld w IO) => App w s -> IO ()
start app@App{..} = runManaged $ Vulkan.run (Vulkan.WindowCreateInfo {Vulkan.sdlWindowConfig = appWindowConfig, Vulkan.logger = def,..}) $ \windowIORef render -> do
    window@Vulkan.VulkanWindow{..} <- readIORef windowIORef
    targetRefreshRate <- case appTargetRefreshRate of
        Just r  -> pure r
        Nothing -> Vulkan.getDisplayModeRefreshRate window def
    res   <- SDL.get $ SDL.windowSize vwSDLWindow
    let initAssets = insertLoaderResource vwSDLRenderer appInitAssets
    (_threadID, finAssets) <- Verda.Asset.Internal.forkAssetLoader initAssets
    world <- appWorld
    time  <- SDL.time
    runWith world $ do
        global $= finAssets
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
        let loop !history !startups !systems !bkgStartups !bkgSystems !lastRenderTime !lastTime = do
                window' <- liftIO $ readIORef windowIORef
                global  $= Window window'
                res'    <- SDL.get $ SDL.windowSize (Vulkan.vwSDLWindow window')
                global  $= WindowResolution (fromIntegral <$> res')
                handleEvents
                newTime <- SDL.time
                Verda.Asset.Internal.updateSingleThreaded finAssets
                global  $= Time (newTime - lastTime) newTime
                let context = SysContext (peekState history)
                bkgStartups' <- filterM (fmap (==Again) . ($context)) bkgStartups
                (history', startups', bkgSystems') <- runSystemsQueue app context history bkgSystems startups
                (history'', startups'', systems')  <- runSystems app history' startups' systems
                TargetRefreshRate refreshRate <- get global
                newRenderTime <- runIfReady refreshRate lastRenderTime (liftIO $ runManaged render)
                quit    <- shouldQuit <$> get global
                if quit
                then let context' = SysContext (peekState history')
                      in mapM_ ($context') $ fromMaybe [] (Map.lookup InBackground appFinalizers)
                else loop history'' startups'' systems' bkgStartups' bkgSystems' newRenderTime newTime
        loop (mkStateHistory appInitStateID)
             (startupsFor app appInitStateID)
             (systemsFor app appInitStateID)
             (fromMaybe [] (Map.lookup InBackground appStartups))
             (fromMaybe [] (Map.lookup InBackground appSystems))
             0 =<< SDL.time

runSystems :: Ord s => App w s -> StateHistory s -> [StateStartup w s] -> [StateSystem w s] -> SystemT w IO (StateHistory s, [StateStartup w s], [StateSystem w s])
runSystems app !history !startups !systems = do
    let context = SysContext $ peekState history
    (history', startups', systems') <- filterM (fmap (==Again) . ($context)) startups >>= \case
        []        -> runSystemsQueue app context history systems []
        startups' -> pure (history, startups', systems)
    pure (history', startups', systems')

runSystemsQueue :: Ord s => App w s -> SysContext s -> StateHistory s -> [StateSystem w s] -> [StateStartup w s] -> SystemT w IO (StateHistory s, [StateStartup w s], [StateSystem w s])
runSystemsQueue app context history systems startups = go systems []
    where go [] done         = pure (history, startups, done)
          go (sys:rest) done = sys context >>= \case 
            Continue   -> go rest (sys:done)
            RemoveThis -> go rest done
            PushState s' -> do
                mapM_ ($context) (finalizersFor app (currentState context))
                pure (pushState s' history, startupsFor app s', systemsFor app s')
            PopState -> do
                let history' = popState_ history
                    prevID   = peekState history'
                mapM_ ($context) (finalizersFor app (currentState context))
                pure (history', startupsFor app prevID, systemsFor app prevID)


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