module Verda.Data.Timer where

data Timer = Timer
    { duration    :: {-# UNPACK #-} !Float
    , elapsedTime :: {-# UNPACK #-} !Float
    } deriving (Eq, Ord, Read, Show)

isFinished :: Timer -> Bool
isFinished Timer{..} = elapsedTime >= duration

mkTimer :: Float -> Timer
mkTimer duration = Timer {elapsedTime = 0,..}

resetTimer :: Timer -> Timer
resetTimer t = t {elapsedTime = 0}

tickTimer :: Float -> Timer -> Timer
tickTimer dT t@Timer{..} = t {elapsedTime = elapsedTime + dT}