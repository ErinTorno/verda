module Verda.Graphics.Components
    ( Camera(..)
    , ClearColor(..)
    , RenderPosition(..)
    , RenderTime(..)
    , TargetRefreshRate(..)
    , Tint(..)
    , WindowResolution(..)
    ) where

import           Apecs
import           Data.Default
import           Data.Hashable
import           GHC.Generics
import           Linear.V3

import           Verda.Data.Components (Time(..))
import           Verda.Graphics.Camera (Camera(..), WindowResolution(..))
import           Verda.Graphics.Color

newtype ClearColor = ClearColor {unClearColor :: RGBA} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Semigroup ClearColor where (<>) = mappend
instance Monoid    ClearColor where mempty = ClearColor $ rgba (Name @"black")
instance Component ClearColor where type Storage ClearColor = Global ClearColor
instance Default ClearColor where
    def = mempty

newtype RenderPosition = RenderPosition {unRenderPosition :: V3 Float} deriving (Eq, Generic, Hashable, Ord, Num, Read, Show)
instance Component RenderPosition where
    type Storage RenderPosition = Map RenderPosition
instance Default RenderPosition where
    def = RenderPosition 0

newtype RenderTime = RenderTime {unRenderTime :: Time} deriving (Eq, Ord, Read, Show)
instance Semigroup RenderTime where (<>) = mappend
instance Monoid    RenderTime where mempty = RenderTime mempty
instance Component RenderTime where type Storage RenderTime = Global RenderTime

newtype TargetRefreshRate = TargetRefreshRate {unTargetRefreshRate :: Float} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Semigroup TargetRefreshRate where (<>) = mappend
instance Monoid    TargetRefreshRate where mempty = TargetRefreshRate 144.0
instance Component TargetRefreshRate where type Storage TargetRefreshRate = Global TargetRefreshRate
instance Default TargetRefreshRate where
    def = mempty

newtype Tint = Tint {unTint :: RGBA} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Component Tint where
    type Storage Tint = Map Tint
instance Default Tint where
    def = Tint $ rgba (Name @"white")