module Verda.Graphics.Components
    ( Camera(..)
    , ClearColor(..)
    , LoadIcon(..)
    , RenderPosition(..)
    , RenderTime(..)
    , TargetRefreshRate(..)
    , Tint(..)
    , Window(..)
    , WindowResolution(..)
    ) where

import           Apecs
import           Data.Default
import           Data.Hashable
import           GHC.Generics
import           Linear.V3

import           Verda.Asset.Types
import           Verda.Data.Components        (Time(..))
import           Verda.Graphics.Camera        (Camera(..), WindowResolution(..))
import           Verda.Graphics.Color
import           Verda.Graphics.Texture
import           Verda.Graphics.Vulkan.Types  (VulkanWindow)

newtype ClearColor = ClearColor {unClearColor :: RGBA} deriving (Eq, Generic, Hashable, Ord, Read, Show)
instance Semigroup ClearColor where (<>) = mappend
instance Monoid    ClearColor where mempty = ClearColor $ rgba (Name @"black")
instance Component ClearColor where type Storage ClearColor = Global ClearColor
instance Default ClearColor where
    def = mempty

newtype LoadIcon = LoadIcon {unLoadIcon :: Maybe (Handle Icon)} deriving (Eq, Ord, Read, Show)
instance Semigroup LoadIcon where (<>) = mappend
instance Monoid    LoadIcon where mempty = LoadIcon Nothing
instance Component LoadIcon where type Storage LoadIcon = Global LoadIcon

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

newtype Window = Window {unWindow :: VulkanWindow}
instance Semigroup Window where (<>) = mappend
instance Monoid    Window where mempty = error "Attempted to access Window before it has been initialize (this is an error and should never happen)"
instance Component Window where type Storage Window = Global Window