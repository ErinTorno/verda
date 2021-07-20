module Verda.Graphics.TextureSpec where

import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import           Data.Default
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as Vec
import qualified SDL
import           Test.Hspec

import           Verda.Asset
import           Verda.Asset.Internal    (nextHandleIdx)
import           Verda.Asset.Types
import           Verda.Graphics.Texture
import           Verda.Test.Utils


spec :: Spec
spec = context "Verda.Graphics.Texture" $ do
    autoWithScaleQualityTest
    autoWithTextureConfigTest
    textureLoadAssetTest

autoWithScaleQualityTest :: Spec
autoWithScaleQualityTest =
    describe "autoWith @ScaleQuality" $ do
        it "should decode ScaleNearest" $
            withImports "Texture.ScaleQuality.Nearest" `shouldDecodeTo` ScaleNearest
        it "should decode ScaleLinear" $
            withImports "Texture.ScaleQuality.Linear" `shouldDecodeTo` ScaleLinear
        it "should decode ScaleAnistropic" $
            withImports "Texture.ScaleQuality.Anistropic" `shouldDecodeTo` ScaleAnistropic
            

autoWithTextureConfigTest :: Spec
autoWithTextureConfigTest =
    describe "autoWith @ScaleQuality" $ do
        it "should decode TextureConfig" $
            withImports "{file = \"tex.png\", scaleQuality = Texture.ScaleQuality.Linear}" `shouldDecodeTo` TextureConfig "tex.png" ScaleLinear
        it "should decode from sample file" $ do
            "./test/assets/icon.tex.dhall" `shouldDecodeTo` TextureConfig "icon.png" ScaleNearest

textureLoadAssetTest :: Spec
textureLoadAssetTest =
    describe "loadAsset @(TextureLoader Texture)" $ do
        let shouldLoad texFile = do
                bs      <- BS.readFile texFile
                withRenderer $ \renderer -> do
                    assets  <- mkAssets renderer
                    when False $ do
                        handle  <- Handle <$> nextHandleIdx assets
                        texture <- runReaderT (unContext $ loadAsset (Proxy @(TextureLoader Texture)) (AssetInfo "icon.png" handle) bs) assets
                        case texture of
                            LoadSuccess LoadedAsset{..} -> dependencies `shouldBe` Vec.empty
                            LoadFailure err             -> failWith $ concat ["Unexpected error `", err, "`"]
                            HandleAlias h               -> failWith $ concat ["Unexpected alias `", show h, "`"]
        it "should parse a png file (using the normal decode fn)" $
            shouldLoad "test/assets/icon.png"
        it "should parse a tga file (using the tga decode fn)" $
            shouldLoad "test/assets/icon.tga"
        it "should parse a tex.dhall file" $ do
            bs      <- BS.readFile "test/assets/icon.tex.dhall"
            withRenderer $ \renderer -> do
                assets  <- mkAssets renderer
                when False $ do
                    handle  <- Handle <$> nextHandleIdx assets
                    texture <- runReaderT (unContext $ loadAsset (Proxy @(TextureLoader Texture)) (AssetInfo "icon.tex.dhall" handle) bs) assets
                    case texture of
                        HandleAlias h             -> h `shouldNotBe` handle
                        LoadSuccess LoadedAsset{} -> failWith "Expected HandleAlias, found LoadSuccuss"
                        LoadFailure err           -> failWith $ concat ["Expected HandleAlias, found LoadFailure \"", err, "\""]

-- -----------
-- -- Utils --
-- -----------

withImports :: Text -> Text
withImports expr = "let Texture = ./assets/dhall/texture.dhall in " `T.append` expr

mkAssets :: SDL.Renderer -> IO Assets
mkAssets renderer = do
    texQualOverrides <- mkTextureScaleQualityOverrides
    insertLoaderResource renderer . insertLoaderResource ScaleLinear . insertLoaderResource texQualOverrides . insertAssetLoader (TextureLoader @Texture)
        <$> emptyAssets def

withRenderer :: (SDL.Renderer -> IO a) -> IO a
withRenderer action = do
    SDL.initialize [SDL.InitVideo]
    window   <- SDL.createWindow "test" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    r <- action renderer
    SDL.quit
    pure r