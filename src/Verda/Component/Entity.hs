module Verda.Component.Entity where

import           Apecs
import           Data.Word
import           Linear.V3
import           Linear.V4

newtype RenderPosition = RenderPosition {unRenderPosition :: V3 Float} deriving (Eq, Ord, Num, Read, Show)
instance Component RenderPosition where type Storage RenderPosition = Map RenderPosition

newtype Tint = Tint {unTint :: V4 Word8} deriving (Eq, Ord, Num, Read, Show)
instance Component Tint where type Storage Tint = Map Tint