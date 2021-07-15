{-# LANGUAGE OverloadedLists #-}

module Verda.Graphics.Texture where

import           Control.Monad.Reader
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified SDL
import qualified SDL.Image

import           Verda.Asset
import qualified Verda.Asset.Internal         as Internal
import           Verda.Asset.Path             (assetExtension)

validExtensions :: Set Text
validExtensions = ["bmp", "cur", "gif", "ico", "jpg", "jpeg", "lbm", "png", "pnm", "pcx", "svg", "tga", "tif", "tiff", "webp", "xpm", "xv"]

data Texture = Texture
    { sdlTexture      :: !SDL.Texture
    , surfaceProvider :: !(Handle (IO SDL.Surface)) -- sometimes still used internally (like with icons), but in most cases we don't want to load Surfaces into memory unless necessary
    }

data TextureLoader r = TextureLoader

instance TextureLoader `CanLoad` Texture where
    extensions _ = validExtensions
    loadAsset  _ info@(AssetInfo path) bytes = withResource $ \renderer ->
        let -- tga needs special logic since it doesn't use a header
            chooseDec :: a -> a -> a
            chooseDec a b = case assetExtension path of {"tga" -> a; _ -> b}
         in do assets <- ask
               tex    <- chooseDec SDL.Image.decodeTextureTGA SDL.Image.decodeTexture renderer bytes
               let provider = do
                       -- re-read bytes since this use case is rare, but will prevent `bytes` from being kept in memory
                       bytes' <- Internal.readAssetFileBytes assets path
                       chooseDec SDL.Image.decodeTGA SDL.Image.decode bytes'
               surHandle <- labeled info "ioSurface" $ LoadedAsset provider []
               pure . simpleSuccess $ Texture
                   { sdlTexture      = tex
                   , surfaceProvider = surHandle
                   }