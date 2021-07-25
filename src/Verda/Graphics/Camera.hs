module Verda.Graphics.Camera where

import           Apecs
import           Data.Default
import           Data.Hashable
import           Data.Word
import           Data.Int
import           GHC.Generics
import           Linear.V2
import           Linear.V3

newtype Camera = Camera {camSize :: V2 Float} deriving (Eq, Generic, Read, Show)
instance Semigroup Camera where (<>) = mappend
instance Monoid    Camera where
    -- | Defaults to 1 unit per pixel
    mempty = Camera $ V2 1280  720
instance Component Camera where type Storage Camera = Unique Camera
instance Default Camera where
    -- | Defaults to 1 unit per pixel
    def = mempty

newtype WindowResolution = WindowResolution {unWindowResolution :: V2 Word32} deriving (Eq, Generic, Hashable, Ord, Num, Read, Show)
instance Semigroup WindowResolution where (<>) = (+)
instance Monoid    WindowResolution where mempty = WindowResolution (floor <$> camSize def)
instance Component WindowResolution where type Storage WindowResolution = Global WindowResolution
instance Default WindowResolution where
    def = mempty

pixelToWorldCoord :: Camera -> V3 Float -> WindowResolution -> V2 Int32 -> V2 Float
pixelToWorldCoord (Camera (V2 camW camH)) (V3 camX camY _) (WindowResolution (V2 resX resY)) (V2 pixX pixY) = V2 ((fromIntegral pixX - camX) / pixPerW) ((fromIntegral pixY - camY) / pixPerH)
    where pixPerW = fromIntegral resX / camW
          pixPerH = fromIntegral resY / camH

pixelToScreenRatioScale :: Camera -> WindowResolution -> V2 Float
pixelToScreenRatioScale (Camera (V2 camW camH)) (WindowResolution (V2 resX resY)) = V2 (fromIntegral resX / camW) (fromIntegral resY / camH)