module Verda.Util.Container where

import           Control.Monad.Primitive
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Vector.Generic.Mutable as GMVec

-------------
-- Vectors --
-------------

growIfNeeded :: (PrimMonad m, GMVec.MVector v a) => Int -> Int -> v (PrimState m) a -> m (v (PrimState m) a) 
growIfNeeded len increment v
    | len >= curLen = GMVec.grow v (len + increment)
    | otherwise     = pure v
    where curLen = GMVec.length v

---------
-- Map --
---------

insideOut :: Ord v => Map k v -> Map v k
insideOut = Map.fromList .  fmap (\(k, v) -> (v, k)) . Map.toList