module Verda.Component.Global
    ( ClearColor(..)
    , ShouldQuit(..)
    , Time(..)
    ) where

import Apecs

import Verda.Graphics.Color (Color(..), black)

newtype ClearColor = ClearColor {unClearColor :: Color} deriving (Eq, Ord, Read, Show)
instance Semigroup ClearColor where (<>) = mappend
instance Monoid    ClearColor where mempty = ClearColor black
instance Component ClearColor where type Storage ClearColor = Global ClearColor

newtype ShouldQuit = ShouldQuit {shouldQuit :: Bool} deriving (Eq, Ord, Read, Show)
instance Semigroup ShouldQuit where (ShouldQuit a) <> (ShouldQuit b) = ShouldQuit $ a && b
instance Monoid    ShouldQuit where mempty = ShouldQuit False
instance Component ShouldQuit where type Storage ShouldQuit = Global ShouldQuit

data Time = Time {tickTime :: !Float, elapsedTime :: !Float} deriving (Eq, Ord, Read, Show)
instance Semigroup Time where (<>) = mappend
instance Monoid    Time where mempty = Time 0 0
instance Component Time where type Storage Time = Global Time