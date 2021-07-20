module Verda.Graphics.Components
    ( ClearColor(..)
    , RenderPosition(..)
    , Tint(..)
    , WindowResolution(..)
    ) where

import           Apecs
import           Data.Default
import           Data.Hashable
import           Data.Word
import           GHC.Generics
import           Linear.V2
import           Linear.V3

import           Verda.Graphics.Color (Color(..), black, white)

newtype Camera = Camera {camSize :: V2 Float} deriving (Eq, Generic, Read, Show)
instance Semigroup Camera where (<>) = mappend
instance Monoid    Camera where
    -- | Defaults to 1 unit per pixel
    mempty = Camera (fromIntegral <$> unWindowResolution def)
instance Component Camera where type Storage Camera = Unique Camera
instance Default Camera where
    -- | Defaults to 1 unit per pixel
    def = mempty

newtype ClearColor = ClearColor {unClearColor :: Color} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Semigroup ClearColor where (<>) = mappend
instance Monoid    ClearColor where mempty = ClearColor black
instance Component ClearColor where type Storage ClearColor = Global ClearColor
instance Default ClearColor where
    def = mempty

newtype RenderPosition = RenderPosition {unRenderPosition :: V3 Float} deriving (Eq, Generic, Hashable, Ord, Num, Read, Show)
instance Component RenderPosition where
    type Storage RenderPosition = Map RenderPosition
instance Default RenderPosition where
    def = RenderPosition 0

newtype Tint = Tint {unTint :: Color} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Component Tint where
    type Storage Tint = Map Tint
instance Default Tint where
    def = Tint white

newtype WindowResolution = WindowResolution {unWindowResolution :: V2 Word32} deriving (Eq, Generic, Hashable, Ord, Num, Read, Show)
instance Semigroup WindowResolution where (<>) = (+)
instance Monoid    WindowResolution where mempty = WindowResolution $ V2 1280  720
instance Component WindowResolution where type Storage WindowResolution = Global WindowResolution
instance Default WindowResolution where
    def = mempty