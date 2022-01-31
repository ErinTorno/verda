module Verda.Graphics.SDL where

import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Marshal.Utils
import           Foreign.Storable
import qualified SDL
import qualified SDL.Internal.Types     as SDL
import qualified SDL.Raw.Error          as SDL.Raw
import qualified SDL.Raw.Types          as SDL.Raw
import qualified SDL.Raw.Video          as SDL.Raw

import           Verda.Graphics.Texture
import           Verda.Util.Logger

setWindowIcon :: MonadIO m => SDL.Window -> Icon -> m ()
setWindowIcon (SDL.Window window) (Icon (SDL.Surface surPtr _)) = SDL.Raw.setWindowIcon window surPtr

getDisplayModeRefreshRate :: MonadIO m => Logger -> Float -> SDL.Window -> m Float
getDisplayModeRefreshRate logger defRate (SDL.Window winPtr) = liftIO $ do
    idx <- SDL.Raw.getWindowDisplayIndex winPtr
    with (SDL.Raw.DisplayMode 0 0 0 0 nullPtr) $ \display -> do
        errCode <- SDL.Raw.getCurrentDisplayMode idx display
        if errCode < 0
        then do
            err <- peekCString =<< SDL.Raw.getError
            logLineWith logger Error $ "getDisplayMode error: " <> T.pack err
            pure defRate
        else
            fromIntegral . SDL.Raw.displayModeRefreshRate <$> peek display