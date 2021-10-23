module Verda.App.Internal where

import           Apecs                        hiding (Map)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Text                    (Text)
import           Data.Typeable
import qualified SDL

import           Verda.Asset
import qualified Verda.Asset.Internal
import           Verda.Event.Control.Internal (mkControlState)
import           Verda.Event.Handler          (handleEvents)
import           Verda.Graphics.Components
import           Verda.Graphics.Texture
import qualified Verda.Graphics.Vulkan.Window as Vulkan
import           Verda.Util.Logger            (Logger)
import           Verda.World

type StateStartup w = SystemT w IO Bool

type StateSystem w s = s -> SystemT w IO s

type StateFinalizer w = SystemT w IO ()

data App w s = App
    { appTitle             :: !Text
    , appWindowConfig      :: !SDL.WindowConfig
    , appWorld             :: !(IO w)
    , appStartups          :: !(Map s [StateStartup w])
    , appSystems           :: !(Map s [StateSystem w s])
    , appFinalizers        :: !(Map s [StateFinalizer w])
    , appInitStateID       :: !s
    , appAssets            :: !Assets
    , appTargetRefreshRate :: !(Maybe Float)
    , appTargetTickRate    :: !(Maybe Float) -- TODO implement
    , appLogger            :: !Logger
    }

makeApp :: MonadIO m => IO w -> m (App w ())
makeApp = makeAppWith def

makeAppWith :: MonadIO m => AssetSettings -> IO w -> m (App w ())
makeAppWith settings mkWorld = do
    texQualOverrides <- mkTextureScaleQualityOverrides
    assets <- insertLoaderResource ScaleLinear . insertLoaderResource texQualOverrides . insertAssetLoader (TextureLoader @Texture)
               <$> emptyAssets settings
    let conf = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.VulkanContext
                                 , SDL.windowInitialSize = fromIntegral <$> unWindowResolution def}
    pure $ App "Untitled App" conf mkWorld Map.empty Map.empty Map.empty () assets Nothing (Just 120) def

-- Setup --

updateWindowConfig :: (SDL.WindowConfig -> SDL.WindowConfig) -> App w s -> App w s
updateWindowConfig f app = app {appWindowConfig = f (appWindowConfig app)}

withTitle :: Text -> App w s -> App w s
withTitle title app = app {appTitle = title}

withAssetLoader :: (a `CanLoad` r, Typeable a, Typeable r) => a r -> App w s -> App w s
withAssetLoader loader app = app {appAssets = insertAssetLoader loader $ appAssets app}

withLoaderResource :: Typeable a => a -> App w s -> App w s
withLoaderResource val app = app {appAssets = insertLoaderResource val $ appAssets app}

withInitState :: s -> App w o -> App w s
withInitState i app = app {appInitStateID = i, appStartups = Map.empty, appSystems = Map.empty, appFinalizers = Map.empty}

withStartup :: Ord s => s -> StateStartup w -> App w s -> App w s
withStartup i sys app = app {appStartups = Map.alter addStartup i (appStartups app)}
    where addStartup (Just l) = Just $ sys:l
          addStartup Nothing  = Just [sys]

withSystem :: Ord s => s -> StateSystem w s -> App w s -> App w s
withSystem st sys app = app {appSystems = Map.alter addSystem st (appSystems app)}
    where addSystem (Just l) = Just $ sys:l
          addSystem Nothing  = Just [sys]

withFinalizer :: Ord s => s -> StateFinalizer w -> App w s -> App w s
withFinalizer st sys app = app {appFinalizers = Map.alter addFinalizer st (appFinalizers app)}
    where addFinalizer (Just l) = Just $ sys:l
          addFinalizer Nothing  = Just [sys]

withTargetRefreshRate :: Maybe Float -> App w s -> App w s
withTargetRefreshRate rate app = app {appTargetRefreshRate = rate}

withLogger :: Logger -> App w s -> App w s
withLogger logger app = app {appLogger = logger}

-- Running --

-- | Starts the given app and runs its systems and asset loaders until the ShouldQuit global is True
start :: Ord s => (VerdaWorld w IO) => App w s -> IO ()
start app@App{..} = Vulkan.run appTitle appWindowConfig def $ \window@Vulkan.VulkanWindow{..} render -> do
    targetRefreshRate <- case appTargetRefreshRate of
        Just r  -> pure r
        Nothing -> Vulkan.getDisplayModeRefreshRate window def
    let finAssets = insertLoaderResource vwSDLRenderer appAssets
    void $ Verda.Asset.Internal.forkAssetLoader finAssets
    world <- appWorld
    res   <- SDL.get $ SDL.windowSize vwSDLWindow
    time  <- SDL.time
    runWith world $ do
        global $= appAssets
        global $= appLogger
        global $= WindowResolution (fromIntegral <$> res)
        global $= Time 0 time
        global $= TargetRefreshRate targetRefreshRate
        set global =<< mkControlState
        let loop !s !startups !lastRenderTime !lastTime = do
                handleEvents
                newTime <- SDL.time
                Verda.Asset.Internal.updateSingleThreaded finAssets
                global  $= Time (newTime - lastTime) newTime
                (s', startups') <- runSystems app s startups
                TargetRefreshRate refreshRate <- get global
                newRenderTime <- runIfReady refreshRate lastRenderTime (liftIO render)
                quit    <- shouldQuit <$> get global
                unless quit $
                    loop s' startups' newRenderTime newTime
        loop appInitStateID (fromMaybe [] $ Map.lookup appInitStateID appStartups) 0 =<< SDL.time

runSystems :: Ord s => App w s -> s -> [StateStartup w] -> SystemT w IO (s, [StateStartup w])
runSystems App{..} !stID = runStartups
    where runStartups startups = filterM (fmap not) startups >>= \case
              []   -> case Map.lookup stID appSystems of
                  Nothing -> pure (stID, [])
                  Just systems -> run stID systems
              rest -> pure (stID, rest)
          run s []  = pure (s, [])
          run s (sys:rest) = sys s >>= \s' ->
              if   s == s'
              then run s' rest
              else do
                  maybe (pure ()) sequence_ $
                      Map.lookup stID appFinalizers
                  maybe (pure (s', [])) runStartups $
                      Map.lookup stID appStartups

runIfReady :: Float -> Float -> SystemT w IO () -> SystemT w IO Float
runIfReady !cappingRatePerSec !lastTime action
    | cappingRatePerSec <= 0.0 = SDL.time
    | otherwise = do
        let delay = 1.0 / cappingRatePerSec
        startTime <- SDL.time
        if startTime - lastTime >= delay
        then action >> SDL.time
        else pure lastTime