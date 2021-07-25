module Verda.App
    ( App(..)
    , makeApp
    , makeAppWith
    , start
    , updateWindowConfig
    , withTitle
    , withAssetLoader
    , withLoaderResource
    , withInitState
    , withStartup
    , withFinalizer
    , withSystem
    ) where

import           Apecs                        hiding (Map)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)
import           Data.Typeable
import qualified SDL

import           Verda.Asset
import qualified Verda.Asset.Internal
import           Verda.Event.Control.Internal (mkControlState)
import           Verda.Event.Handler          (handleEvents)
import Verda.Graphics.Texture
import           Verda.World

data App w s = App
    { appTitle         :: !Text
    , appWindowConfig  :: !SDL.WindowConfig
    , appWorld         :: !(IO w)
    , appStartups      :: !(Map s [s -> SystemT w IO s])
    , appSystems       :: !(Map s [s -> SystemT w IO s])
    , appFinalizers    :: !(Map s [s -> SystemT w IO s])
    , appInitStateID   :: !s
    , appAssets        :: !Assets
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
    pure $ App "Untitled App" conf mkWorld Map.empty Map.empty Map.empty () assets

-- Setup --

updateWindowConfig :: (SDL.WindowConfig -> SDL.WindowConfig) -> App w s -> App w s
updateWindowConfig f app = app {appWindowConfig = f (appWindowConfig app)}

withTitle :: Text -> App w s -> App w s
withTitle title app = app {appTitle = title}

withAssetLoader :: (a `CanLoad` r, Typeable r) => a r -> App w s -> App w s
withAssetLoader loader app = app {appAssets = insertAssetLoader loader $ appAssets app}

withLoaderResource :: Typeable a => a -> App w s -> App w s
withLoaderResource val app = app {appAssets = insertLoaderResource val $ appAssets app}

withInitState :: s -> App w o -> App w s
withInitState i app = app {appInitStateID = i, appStartups = Map.empty, appSystems = Map.empty, appFinalizers = Map.empty}

withStartup :: Ord s => s -> (s -> SystemT w IO s) -> App w s -> App w s
withStartup i sys app = app {appStartups = Map.alter addStartup i (appStartups app)}
    where addStartup (Just l) = Just $ sys:l
          addStartup Nothing  = Just [sys]

withSystem :: Ord s => s -> (s -> SystemT w IO s) -> App w s -> App w s
withSystem st sys app = app {appSystems = Map.alter addSystem st (appSystems app)}
    where addSystem (Just l) = Just $ sys:l
          addSystem Nothing  = Just [sys]

withFinalizer :: Ord s => s -> (s -> SystemT w IO s) -> App w s -> App w s
withFinalizer st sys app = app {appFinalizers = Map.alter addFinalizer st (appFinalizers app)}
    where addFinalizer (Just l) = Just $ sys:l
          addFinalizer Nothing  = Just [sys]

-- Running --

-- | Starts the given app and runs its systems and asset loaders until the ShouldQuit global is True
start :: Ord s => (VerdaWorld w IO) => App w s -> IO ()
start app@App{..} = do
    SDL.initializeAll
    window   <- SDL.createWindow appTitle appWindowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.showWindow window
    void $ Verda.Asset.Internal.forkAssetLoader appAssets
    --  $ insertLoaderResource renderer appAssets
    world <- appWorld
    res   <- SDL.get $ SDL.windowSize window
    time  <- SDL.time
    runWith world $ do
        global $= appAssets
        global $= WindowResolution (fromIntegral <$> res)
        global $= Time time 0
        set global =<< mkControlState
        newSt  <- runSystems appInitStateID appStartups app
        let loop s lastTime = do
                handleEvents
                newTime <- SDL.time
                global  $= Time newTime (newTime - lastTime)
                s'      <- runSystems s appSystems app
                quit    <- shouldQuit <$> get global
                unless quit $
                    loop s' newTime
        loop newSt =<< SDL.time
    SDL.quit

runSystems :: Ord s => s -> Map s [s -> SystemT w IO s] -> App w s -> SystemT w IO s
runSystems stID group App{..} = run stID (systems stID group)
    where systems = Map.findWithDefault [] 
          run s []  = pure s
          run s (sys:rest) = sys s >>= \s' -> if s == s'
              then run s' rest
              else do
                  s'' <- run s' $ systems s' appFinalizers
                  if   s' == s''
                  then run s' $ systems s' appStartups
                  else pure s''