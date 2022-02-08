{-# LANGUAGE OverloadedLists #-}

module Verda.Graphics.Texture
    ( Icon(..)
    , ScaleQuality(..)
    , Texture(..)
    , TextureConfig(..)
    , TextureLoader(..)
    , TextureScaleQualityOverrides
    , textureBundle
    ) where

import           Control.Monad.Reader
import           Control.Monad.ST             (RealWorld, stToIO)
import           Data.ByteString              (ByteString)
import           Data.Default
import           Data.Dynamic
import           Data.HashTable.ST.Basic      (HashTable)
import qualified Data.HashTable.ST.Basic      as HT
import           Data.Maybe
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Dhall
import           GHC.Generics
import qualified SDL
import qualified SDL.Image

import           Verda.Asset
import           Verda.Asset.Internal
import           Verda.Asset.Interop
import           Verda.Asset.Path             (assetExtension)

validExtensions :: Set Text
validExtensions = ["bmp", "cur", "gif", "ico", "jpg", "jpeg", "lbm", "png", "pnm", "pcx", "svg", "tga", "tif", "tiff", "webp", "xpm", "xv", "tex.dhall"]

data ScaleQuality = ScaleNearest | ScaleLinear | ScaleAnistropic deriving (Enum, Eq, Generic, Ord, Read, Show)

instance Default ScaleQuality where def = ScaleLinear

instance Dhall.FromDhall ScaleQuality where
    autoWith _ = Dhall.genericAutoWith (Dhall.defaultInterpretOptions {Dhall.constructorModifier = \n -> fromMaybe n $ T.stripPrefix "Scale" n})

toRenderScaleQuality :: ScaleQuality -> SDL.RenderScaleQuality
toRenderScaleQuality q = case q of
    ScaleNearest    -> SDL.ScaleNearest
    ScaleLinear     -> SDL.ScaleLinear
    ScaleAnistropic -> SDL.ScaleBest

data Image

newtype TextureScaleQualityOverrides = TextureScaleQualityOverrides (HashTable RealWorld (Handle Image) ScaleQuality)

instance ResourceGenerator TextureScaleQualityOverrides where
    genResource = TextureScaleQualityOverrides <$> stToIO HT.new

data TextureConfig = TextureConfig
    { file         :: !Path
    , scaleQuality :: !ScaleQuality
    } deriving (Eq, Generic, Read, Show)
instance Dhall.FromDhall TextureConfig

newtype Icon = Icon {unIcon :: SDL.Surface}

newtype Texture = Texture {unTexture :: SDL.Texture}

data TextureLoader r = TextureLoader deriving (Eq, Ord, Generic, Read, Show)

instance Default (TextureLoader r) where def = TextureLoader

textureBundle :: Bundled [ Resource  ScaleQuality
                         , ResourceM TextureScaleQualityOverrides
                         , Loader    (TextureLoader Icon)
                         , Loader    (TextureLoader Texture)
                         ]
textureBundle = BundledProxy

instance TextureLoader `CanLoad` Icon where
    extensions _ = validExtensions
    loadAsset  _ info = loadImageAsset (\case {"tga" -> const SDL.Image.decodeTGA; _ -> const SDL.Image.decode}) Icon info

instance TextureLoader `CanLoad` Texture where
    -- | True, as SDL.Renderer requires a single-threaded environment
    isSingleThreadOnly _ = True
    extensions _ = validExtensions
    loadAsset  _ info = loadImageAsset (\case {"tga" -> SDL.Image.decodeTextureTGA; _ -> SDL.Image.decodeTexture}) Texture info

withScaleQuality :: MonadIO m => ScaleQuality -> m a -> m a
withScaleQuality quality action = do
    defScaling <- SDL.get SDL.HintRenderScaleQuality
    SDL.HintRenderScaleQuality SDL.$= toRenderScaleQuality quality
    r <- action
    SDL.HintRenderScaleQuality SDL.$= defScaling
    pure r

loadImageAsset :: Typeable b => (Text -> SDL.Renderer -> ByteString -> LoadContext a) -> (a -> b) -> AssetLoadFn b
loadImageAsset decode constr AssetInfo{..} bytes = case assetExtension assetPath of
    "tex.dhall" ->
        parseFromDhall bytes >>= \case
            Left err                -> pure $ simpleFailure err
            Right TextureConfig{..} ->
                withResource $ \defQuality -> withResource $ \(TextureScaleQualityOverrides overrides) -> do
                    tex <-
                        if   scaleQuality == defQuality
                        then getHandle file
                        else getHandleWith file $ \handle -> liftIO $ stToIO $ HT.insert overrides (coerceHandle handle) scaleQuality
                    pure $ simpleAlias tex
    ext -> withResource $ \renderer -> withResource $ \scaleQuality -> withResource $ \(TextureScaleQualityOverrides overrides) -> do
        scaling <- fromMaybe scaleQuality <$> liftIO (stToIO $ HT.lookup overrides (coerceHandle assetHandle))
        withScaleQuality scaling $ do
            tex    <- decode ext renderer bytes
            pure . simpleSuccess . constr $ tex