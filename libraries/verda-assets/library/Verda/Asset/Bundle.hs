{-# LANGUAGE UndecidableInstances #-}

module Verda.Asset.Bundle where

import           Data.Default
import           Data.Functor.Identity
import           Data.Proxy
import           Type.Reflection

import           Verda.Asset.Internal   (insertAssetLoader, insertLoaderResource, insertLoaderResourceM)
import           Verda.Asset.Types

class LoaderBundle a where
    -- | Updates the given Assets to support all the bundle's loaders
    insertBundle :: proxy a -> Assets -> Assets

type family BundleList rs where
     BundleList '[]       = ()
     BundleList (r ': rs) = (r, BundleList rs)

class ResourceGenerator r where
     genResource :: IO r

data Resource a

data ResourceM a

data Loader a

data BundledProxy rs = BundledProxy

instance Default (BundledProxy r) where
     def = BundledProxy

type family Bundled rs where
     Bundled rs = BundledProxy (BundleList rs)

---------------
-- Instances --
---------------

instance LoaderBundle () where
     insertBundle _ = id

instance ( LoaderBundle a
         ) => LoaderBundle (Identity a) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @a) proxy)

instance ( Default a
         , Typeable a
         ) => LoaderBundle (Resource a) where
     insertBundle _ = insertLoaderResource $ def @a

instance ( ResourceGenerator a
         , Typeable a
         ) => LoaderBundle (ResourceM a) where
     insertBundle _ = insertLoaderResourceM $ genResource @a

instance ( a `CanLoad` ar
         , Default (a ar)
         , Typeable ar
         ) => LoaderBundle (Loader (a ar)) where
     insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)

instance ( LoaderBundle a
         , LoaderBundle b
         ) => LoaderBundle (a, b) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         ) => LoaderBundle (a, b, c) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         ) => LoaderBundle (a, b, c, d) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         ) => LoaderBundle (a, b, c, d, e) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         , LoaderBundle f
         ) => LoaderBundle (a, b, c, d, e, f) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @f) proxy)
                        . insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         , LoaderBundle f
         , LoaderBundle g
         ) => LoaderBundle (a, b, c, d, e, f, g) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @g) proxy)
                        . insertBundle ((\_ -> Proxy @f) proxy)
                        . insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         , LoaderBundle f
         , LoaderBundle g
         , LoaderBundle h
         ) => LoaderBundle (a, b, c, d, e, f, g, h) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @h) proxy)
                        . insertBundle ((\_ -> Proxy @g) proxy)
                        . insertBundle ((\_ -> Proxy @f) proxy)
                        . insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         , LoaderBundle f
         , LoaderBundle g
         , LoaderBundle h
         , LoaderBundle i
         ) => LoaderBundle (a, b, c, d, e, f, g, h, i) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @i) proxy)
                        . insertBundle ((\_ -> Proxy @h) proxy)
                        . insertBundle ((\_ -> Proxy @g) proxy)
                        . insertBundle ((\_ -> Proxy @f) proxy)
                        . insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         , LoaderBundle c
         , LoaderBundle d
         , LoaderBundle e
         , LoaderBundle f
         , LoaderBundle g
         , LoaderBundle h
         , LoaderBundle i
         , LoaderBundle j
         ) => LoaderBundle (a, b, c, d, e, f, g, h, i, j) where
     insertBundle proxy = insertBundle ((\_ -> Proxy @j) proxy)
                        . insertBundle ((\_ -> Proxy @i) proxy)
                        . insertBundle ((\_ -> Proxy @h) proxy)
                        . insertBundle ((\_ -> Proxy @g) proxy)
                        . insertBundle ((\_ -> Proxy @f) proxy)
                        . insertBundle ((\_ -> Proxy @e) proxy)
                        . insertBundle ((\_ -> Proxy @d) proxy)
                        . insertBundle ((\_ -> Proxy @c) proxy)
                        . insertBundle ((\_ -> Proxy @b) proxy)
                        . insertBundle ((\_ -> Proxy @a) proxy)