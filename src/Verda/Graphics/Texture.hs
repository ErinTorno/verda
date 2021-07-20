{-# LANGUAGE OverloadedLists #-}

module Verda.Graphics.Texture
    ( ScaleQuality(..)
    , Texture(..)
    , TextureConfig(..)
    , TextureLoader(..)
    , TextureScaleQualityOverrides(..)
    , mkTextureScaleQualityOverrides
    ) where
import           Control.Monad.Reader
import           Control.Monad.ST             (RealWorld, stToIO)
import           Data.HashTable.ST.Basic      (HashTable)
import qualified Data.HashTable.ST.Basic      as HT
import           Data.Maybe
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Manipulate         as T
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
instance Dhall.FromDhall ScaleQuality where
    autoWith _ = Dhall.genericAutoWith (Dhall.defaultInterpretOptions {Dhall.constructorModifier = \n -> fromMaybe n $ T.stripPrefix "Scale" n})

toRenderScaleQuality :: ScaleQuality -> SDL.RenderScaleQuality
toRenderScaleQuality q = case q of
    ScaleNearest    -> SDL.ScaleNearest
    ScaleLinear     -> SDL.ScaleLinear
    ScaleAnistropic -> SDL.ScaleBest

newtype TextureScaleQualityOverrides = TextureScaleQualityOverrides (HashTable RealWorld (Handle Texture) ScaleQuality)

mkTextureScaleQualityOverrides :: MonadIO m => m TextureScaleQualityOverrides
mkTextureScaleQualityOverrides = fmap TextureScaleQualityOverrides $ liftIO $ stToIO HT.new

data TextureConfig = TextureConfig
    { texConFile         :: !Path
    , texConScaleQuality :: !ScaleQuality
    } deriving (Eq, Generic, Read, Show)
instance Dhall.FromDhall TextureConfig where
    autoWith _ = Dhall.genericAutoWith (Dhall.defaultInterpretOptions {Dhall.fieldModifier = \n -> T.toCamel $ fromMaybe n $ T.stripPrefix "texCon" n})

data Texture = Texture
    { sdlTexture      :: !SDL.Texture
    , surfaceProvider :: !(Handle (IO SDL.Surface)) -- sometimes still used internally (like with icons), but in most cases we don't want to load Surfaces into memory unless necessary
    }

data TextureLoader r = TextureLoader

instance TextureLoader `CanLoad` Texture where
    extensions _ = validExtensions
    loadAsset  _ AssetInfo{..} bytes = case  assetExtension assetPath of
            "tex.dhall" -> do
                parseFromDhall bytes >>= \case
                    Left err     -> pure $ simpleFailure err
                    Right TextureConfig{..} -> withResource $ \defQuality -> withResource $ \(TextureScaleQualityOverrides overrides) -> do
                        tex <-
                            if   texConScaleQuality == defQuality
                            then getHandle texConFile
                            else getHandleWith texConFile $ \handle -> liftIO $ stToIO $ HT.insert overrides handle texConScaleQuality
                        pure $ simpleAlias tex
            ext -> withResource $ \renderer -> withResource $ \scaleQuality -> withResource $ \(TextureScaleQualityOverrides overrides) ->
                let -- tga needs special logic since it doesn't use a header
                    chooseDec :: a -> a -> a
                    chooseDec a b = case ext of {"tga" -> a; _ -> b}
                 in do scaling <- fromMaybe scaleQuality <$> liftIO (stToIO $ HT.lookup overrides assetHandle)
                       withScaleQuality scaling $ do
                           assets <- ask
                           tex    <- chooseDec SDL.Image.decodeTextureTGA SDL.Image.decodeTexture renderer bytes
                           let provider = withScaleQuality scaling $ do
                                   -- re-read bytes since this use case is rare, but will prevent `bytes` from being kept in memory
                                   bytes' <- readAssetFileBytes assets assetPath
                                   chooseDec SDL.Image.decodeTGA SDL.Image.decode bytes'
                           surHandle <- labeled assetPath "surfaceProvider" $ LoadedAsset provider []
                           pure . simpleSuccess $ Texture
                               { sdlTexture      = tex
                               , surfaceProvider = surHandle
                               }

withScaleQuality :: MonadIO m => ScaleQuality -> m a -> m a
withScaleQuality quality action = do
    defScaling <- SDL.get SDL.HintRenderScaleQuality
    SDL.HintRenderScaleQuality SDL.$= toRenderScaleQuality quality
    r <- action
    SDL.HintRenderScaleQuality SDL.$= defScaling
    pure r