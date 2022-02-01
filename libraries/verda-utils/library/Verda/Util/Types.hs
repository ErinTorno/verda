module Verda.Util.Types where

-- ^ A class for one-way non-failing conversions from one type to another
class ConvertTo a b where
    -- | Convert value from `a` to `b`
    convert :: a -> b

instance a `ConvertTo` a where
    convert = id