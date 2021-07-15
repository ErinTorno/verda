module Verda.Graphics.Components
    ( ClearColor(..)
    , RenderPosition(..)
    , Tint(..)
    ) where

import Apecs
import Linear.V3

import Verda.Graphics.Color (Color(..), black)

newtype ClearColor = ClearColor {unClearColor :: Color} deriving (Eq, Ord, Read, Show)
instance Semigroup ClearColor where (<>) = mappend
instance Monoid    ClearColor where mempty = ClearColor black
instance Component ClearColor where type Storage ClearColor = Global ClearColor

newtype RenderPosition = RenderPosition {unRenderPosition :: V3 Float} deriving (Eq, Ord, Num, Read, Show)
instance Component RenderPosition where type Storage RenderPosition = Map RenderPosition

newtype Tint = Tint {unTint :: Color} deriving (Eq, Ord, Read, Show)
instance Component Tint where type Storage Tint = Map Tint