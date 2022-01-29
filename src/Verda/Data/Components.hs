{-# LANGUAGE DuplicateRecordFields #-}

module Verda.Data.Components where

import           Apecs

newtype ShouldQuit = ShouldQuit {shouldQuit :: Bool} deriving (Eq, Ord, Read, Show)
instance Semigroup ShouldQuit where (ShouldQuit a) <> (ShouldQuit b) = ShouldQuit $ a && b
instance Monoid    ShouldQuit where mempty = ShouldQuit False
instance Component ShouldQuit where type Storage ShouldQuit = Global ShouldQuit

data Time = Time
    { tickTime    :: {-# UNPACK #-} !Float
    , elapsedTime :: {-# UNPACK #-} !Float
    } deriving (Eq, Ord, Read, Show)
instance Semigroup Time where (<>) = mappend
instance Monoid    Time where mempty = Time 0 0
instance Component Time where type Storage Time = Global Time