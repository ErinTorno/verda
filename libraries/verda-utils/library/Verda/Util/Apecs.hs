module Verda.Util.Apecs where

import           Apecs
import           Apecs.Core
import           Control.Monad          (when)
import           Data.Kind              (Constraint)
import qualified Data.Vector.Unboxed    as UVec

-----------
-- Types --
-----------

type family ReadWrite w m c :: Constraint where
    ReadWrite w m c = (Get w m c, Has w m c, Set w m c)

type family ReadWriteEach w m cs :: Constraint where
    ReadWriteEach w m '[]       = ()
    ReadWriteEach w m (c ': cs) = (Get w m c, Has w m c, Set w m c, ReadWriteEach w m cs)

--------------------
-- Util functions --
--------------------

{-# INLINE getUnique #-}
getUnique :: forall w m c. (Members w m c, Get w m c) => SystemT w m (Maybe c)
getUnique = cfold (\_ c -> Just c) Nothing

{-# INLINE getIfExists #-}
getIfExists :: forall w m c. Get w m c => Entity -> SystemT w m (Maybe c)
getIfExists !ety = do
    doesExist <- exists ety (Proxy :: Proxy c)
    if doesExist
    then Just <$> get ety
    else pure Nothing

{-# INLINE whenExists #-}
whenExists :: forall w m c. (Members w m c, Get w m c) => Entity -> (c -> SystemT w m ()) -> SystemT w m ()
whenExists !ety !f = do
    doesExist <- exists ety (Proxy :: Proxy c)
    when doesExist (get ety >>= f)

-- Same logic as Apec's cmapIf implementation, just with f returning in a monad
{-# INLINE cmapIfM #-}
cmapIfM :: forall w m cp cx cy. (Get w m cx, Get w m cp, Members w m cx, Set w m cy)
        => (cp -> Bool) -> (cx -> SystemT w m cy) -> SystemT w m ()
cmapIfM !cond !f = do
    pStore :: Storage cp <- getStore
    xStore :: Storage cx <- getStore
    yStore :: Storage cy <- getStore
    cs     <- lift . explMembers $ (xStore, pStore)
    UVec.forM_ cs $ \ety -> do
        p <- lift . explGet pStore $ ety
        when (cond p) $ do
            x <- lift . explGet xStore $ ety
            y <- f x
            lift . explSet yStore ety $ y