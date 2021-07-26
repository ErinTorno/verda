module Verda.Asset.Bundle where

import           Data.Default
import           Data.Functor.Identity
import           Data.Proxy
import           Type.Reflection

import           Verda.Asset.Internal (insertAssetLoader)
import           Verda.Asset.Types

class LoaderBundle a where
    -- | Updates the given Assets to support all the bundle's loaders
    insertBundle :: Proxy a -> Assets -> Assets

data Bundled a = Bundled deriving (Eq, Ord, Read, Show)

instance Default (Bundled a) where
     def = Bundled

instance LoaderBundle () where
     insertBundle _ = id

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         ) => LoaderBundle (Identity (a ar)) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)

-- Bundles of Tuples of Bundles --

instance LoaderBundle (Bundled ()) where
    insertBundle _ = id

instance ( LoaderBundle a
         ) => LoaderBundle (Bundled (Identity a)) where
    insertBundle proxy = insertBundle ((\_ -> Proxy @a) proxy)

instance ( LoaderBundle a
         , LoaderBundle b
         ) => LoaderBundle (Bundled (a, b)) where
    insertBundle proxy = insertBundle ((\_ -> Proxy @a) proxy)
                       . insertBundle ((\_ -> Proxy @b) proxy)

-- Tuples of Loaders --

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         ) => LoaderBundle (a ar, b br) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         ) => LoaderBundle (a ar, b br, c cr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         ) => LoaderBundle (a ar, b br, c cr, d dr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         , h `CanLoad` hr, Default (h hr), Typeable h, Typeable hr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr, h hr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: h hr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         , h `CanLoad` hr, Default (h hr), Typeable h, Typeable hr
         , i `CanLoad` ir, Default (i ir), Typeable i, Typeable ir
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr, h hr, i ir) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: h hr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: i ir) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         , h `CanLoad` hr, Default (h hr), Typeable h, Typeable hr
         , i `CanLoad` ir, Default (i ir), Typeable i, Typeable ir
         , j `CanLoad` jr, Default (j jr), Typeable j, Typeable jr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr, h hr, i ir, j jr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: h hr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: i ir) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: j jr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         , h `CanLoad` hr, Default (h hr), Typeable h, Typeable hr
         , i `CanLoad` ir, Default (i ir), Typeable i, Typeable ir
         , j `CanLoad` jr, Default (j jr), Typeable j, Typeable jr
         , k `CanLoad` kr, Default (k kr), Typeable k, Typeable kr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr, h hr, i ir, j jr, k kr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: h hr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: i ir) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: j jr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: k kr) -> val) proxy def)

instance ( a `CanLoad` ar, Default (a ar), Typeable a, Typeable ar
         , b `CanLoad` br, Default (b br), Typeable b, Typeable br
         , c `CanLoad` cr, Default (c cr), Typeable c, Typeable cr
         , d `CanLoad` dr, Default (d dr), Typeable d, Typeable dr
         , e `CanLoad` er, Default (e er), Typeable e, Typeable er
         , f `CanLoad` fr, Default (f fr), Typeable f, Typeable fr
         , g `CanLoad` gr, Default (g gr), Typeable g, Typeable gr
         , h `CanLoad` hr, Default (h hr), Typeable h, Typeable hr
         , i `CanLoad` ir, Default (i ir), Typeable i, Typeable ir
         , j `CanLoad` jr, Default (j jr), Typeable j, Typeable jr
         , k `CanLoad` kr, Default (k kr), Typeable k, Typeable kr
         , l `CanLoad` lr, Default (l lr), Typeable l, Typeable lr
         ) => LoaderBundle (a ar, b br, c cr, d dr, e er, f fr, g gr, h hr, i ir, j jr, k kr, l lr) where
    insertBundle proxy = insertAssetLoader ((\_ (val :: a ar) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: b br) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: c cr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: d dr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: e er) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: f fr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: g gr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: h hr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: i ir) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: j jr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: k kr) -> val) proxy def)
                       . insertAssetLoader ((\_ (val :: l lr) -> val) proxy def)