module Verda.System.Window where

import           Apecs
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                    as T

import           Verda.Asset
import           Verda.Graphics.Components
import qualified Verda.Graphics.SDL           as SDL
import           Verda.Graphics.Texture
import qualified Verda.Graphics.Vulkan.Types  as Vulkan
import           Verda.Util.Logger
import           Verda.Util.Apecs
import           Verda.World

setAppIcon :: (MonadIO m, ReadWrite w m Window) => Icon -> SystemT w m ()
setAppIcon icon = get global >>= flip SDL.setWindowIcon icon . Vulkan.vwSDLWindow . unWindow

iconSystem :: VerdaWorld w IO => SystemT w IO ()
iconSystem = do
    assets     <- get global
    iconHandle <- unLoadIcon <$> get global
    forM_ iconHandle $ \h -> do
        assetOrStatus assets h >>= \case
            Right icon -> do
                setAppIcon icon
                global $= LoadIcon Nothing
            Left (Failed s) -> do
                logLine Error ("iconSystem: LoadIcon asset failed to load (" <> T.pack s <> ")")
                global $= LoadIcon Nothing
            _        -> pure ()