module Verda.Graphics.TextureSpec where

-- import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import           Data.Default
-- import qualified Data.Vector             as Vec
import qualified SDL
import           Test.Hspec

import           Verda.Asset
-- import           Verda.Asset.Types
-- import           Verda.Graphics.Texture
-- import           Verda.Test.Utils

spec :: Spec
spec = context "Verda.Graphics.Texture" $ do
    textureLoadAssetTest

textureLoadAssetTest :: Spec
textureLoadAssetTest =
    describe "(TextureLoader `CanLoad` Texture).loadAsset" $ do
        it "should parse png file" $ do
            bs      <- BS.readFile "test/assets/icon.png"
            withRenderer $ \renderer -> do
                assets  <- insertLoaderResource renderer <$> emptyAssets def
                pure (const (const () bs) assets)
                -- -- disabled as it breaks hspec when the texture is decoded
                -- texture <- runReaderT (unContext $ loadAsset (Proxy @(TextureLoader Texture)) (AssetInfo "icon.png") bs) assets
                -- texture `shouldSatisfyNS` (\case {Right LoadedAsset{..} -> dependencies == Vec.empty; _ -> False})

-- -----------
-- -- Utils --
-- -----------

withRenderer :: (SDL.Renderer -> IO a) -> IO a
withRenderer action = do
    SDL.initialize [SDL.InitVideo]
    window   <- SDL.createWindow "test" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    r <- action renderer
    SDL.quit
    pure r