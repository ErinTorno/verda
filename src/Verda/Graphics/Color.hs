{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Verda.Graphics.Color
    ( ConvertTo(..)
    , module Verda.Graphics.Color
    ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word
import           Data.Hashable
import           GHC.Generics
import           GHC.TypeLits
import           Linear.V4                    (V4(..))
import           Numeric                      (readHex, showHex)

import           Verda.Util.Types

-----------------
-- Color Types --
-----------------

newtype RGBA = RGBA
    { unRGBA :: V4 Word8 -- ^ RGBA formatted for 0..255
    } deriving (Eq, Ord, Generic, Hashable, Read, Show)

derivingUnbox "RGBA" [t| RGBA -> V4 Word8 |] [| unRGBA |] [| RGBA |]

data HSVA = HSVA
    { hue        :: {-# UNPACK #-} !Float -- ^ 0..360
    , saturation :: {-# UNPACK #-} !Float -- ^ 0.0..1.0
    , value      :: {-# UNPACK #-} !Float -- ^ 0.0..1.0
    , alpha      :: {-# UNPACK #-} !Float -- ^ 0.0..1.0
    } deriving (Eq, Ord, Generic, Read, Show)

---------------
-- ConvertTo --
---------------

instance RGBA `ConvertTo` RGBA where
    convert = id

instance RGBA `ConvertTo` HSVA where
    convert (RGBA vec@(V4 ri gi bi _)) = HSVA h s v af
        where -- we convert from an byte to a float between 1 and zero for intensity
              fromWord :: Word8 -> Float
              fromWord w = fromIntegral w / 255
              -- get max and min components in our color
              maxcomp = max ri $ max bi gi
              mincomp = min ri $ min bi gi
              -- get red, green, and blue intensities
              (V4 rf gf bf af) = fromWord <$> vec
              delta   = fromWord maxcomp - fromWord mincomp
              getHue 0.0 _ = 0.0
              getHue _ m | m == ri   = (gf - bf) / delta
                         | m == gi   = (bf - rf) / delta + 2
                         | otherwise = (rf - gf) / delta + 4
              -- restrict the hue to within 0 and 360
              h = modDegrees . (*60) . getHue delta $ maxcomp
              s = if maxcomp == 0
                  then 0
                  else 1 - (fromIntegral mincomp / fromIntegral maxcomp)
              v = fromWord maxcomp

instance HSVA `ConvertTo` HSVA where
    convert = id

instance HSVA `ConvertTo` RGBA where
    convert hsv =
        let fromFloat :: Float -> Word8
            fromFloat = round . (*255.0)
            alph = fromFloat . alpha $ hsv
            hi   = ((`mod`6) . floor . (/60) . hue $ hsv) :: Int
            f    = (hue hsv / 60) - (fromInteger . floor) (hue hsv / 60)
            v = fromFloat . (* value hsv) $ 1
            p = fromFloat . (* value hsv) $ 1 - saturation hsv
            q = fromFloat . (* value hsv) $ 1 - f * saturation hsv
            t = fromFloat . (* value hsv) $ 1 - (1 - f) * saturation hsv
         in case hi of
            0 -> mkRGBA v t p alph
            1 -> mkRGBA q v p alph
            2 -> mkRGBA p v t alph
            3 -> mkRGBA p q v alph
            4 -> mkRGBA t p v alph
            _ -> mkRGBA v p q alph

--------------------
-- RGBA functions --
--------------------

mkRGB :: Word8 -> Word8 -> Word8 -> RGBA
mkRGB r g b = RGBA $ V4 r g b 255

mkRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
mkRGBA r g b a = RGBA $ V4 r g b a

-- | Parses a hex code of the following forms rrggbb and rrggbbaa with an optional # in front
fromHex :: RGBA `ConvertTo` a => String -> Maybe a
fromHex ('#':hex) = fromHex hex
fromHex s = fmap (convert @RGBA @_) $ case s of
    [r1,r2, g1,g2, b1,b2, a1,a2] -> mkRGBA <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> pair a1 a2
    [r1,r2, g1,g2, b1,b2]        -> mkRGBA <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> Just 255
    _                            -> Nothing
    where pair a b = (\case []         -> Nothing
                            ((x, _):_) -> Just x) . readHex $ [a, b]

-- | Converts the given color to color hex code of the format #rrggbbaa
toHex :: a `ConvertTo` RGBA => a -> String
toHex color = ('#':) . padHex r . padHex g . padHex b . padHex a $ ""
    where RGBA (V4 r g b a) = convert color
          padHex n = (++) . pad . showHex n $ ""
          pad s    = let len = length s in if len <= 2 then replicate (2 - len) '0' ++ s else s

--------------------
-- HSVA functions --
--------------------

mkHSV :: Float -> Float -> Float -> HSVA
mkHSV h s v = HSVA h s v 1

mkHSVA :: Float -> Float -> Float -> Float -> HSVA
mkHSVA = HSVA

-----------------
-- Base colors --
-----------------

class BaseColor (name :: Symbol) a where
    -- | Returns a color for the given name and color format
    colorOf :: proxy a name -> a

data Format f (name :: Symbol) = Format 

data Name (name :: Symbol) = Name

colorFromName :: BaseColor n a => proxy n -> a
colorFromName = colorOf . format
    where format :: proxy n -> Format a n
          format _ = Format

-- | Returns an RGBA color for the given name
rgba :: BaseColor n RGBA => proxy n -> RGBA
rgba = colorFromName @_ @RGBA

-- | Returns an HSVA color for the given name
hsva :: BaseColor n HSVA => proxy n -> HSVA
hsva = colorFromName @_ @HSVA

instance BaseColor "black"   RGBA where colorOf _ = mkRGB 0 0 0
instance BaseColor "black"   HSVA where colorOf _ = mkHSV 0 0 0

instance BaseColor "silver"  RGBA where colorOf _ = mkRGB 0xc0 0xc0 0xc0
instance BaseColor "silver"  HSVA where colorOf _ = mkHSV 0 0 0.735

instance BaseColor "grey"    RGBA where colorOf _ = mkRGB 0x80 0x80 0x80
instance BaseColor "grey"    HSVA where colorOf _ = mkHSV 0 0 0.502

instance BaseColor "white"   RGBA where colorOf _ = mkRGB 0xff 0xff 0xff
instance BaseColor "white"   HSVA where colorOf _ = mkHSV 0 0 1

instance BaseColor "maroon"  RGBA where colorOf _ = mkRGB 0x80 0x00 0x00
instance BaseColor "maroon"  HSVA where colorOf _ = mkHSV 0 1 0.502

instance BaseColor "red"     RGBA where colorOf _ = mkRGB 0xff 0 0
instance BaseColor "red"     HSVA where colorOf _ = mkHSV 0 1 1

instance BaseColor "purple"  RGBA where colorOf _ = mkRGB 0x80 0x00 0x80
instance BaseColor "purple"  HSVA where colorOf _ = mkHSV 300 1 0.502

instance BaseColor "fuchsia" RGBA where colorOf _ = mkRGB 0xff 0x00 0xff
instance BaseColor "fuchsia" HSVA where colorOf _ = mkHSV 300 1 1

instance BaseColor "green"   RGBA where colorOf _ = mkRGB 0x00 0x80 0x00
instance BaseColor "green"   HSVA where colorOf _ = mkHSV 100 1 0.502

instance BaseColor "lime"    RGBA where colorOf _ = mkRGB 0x00 0xff 0x00
instance BaseColor "lime"    HSVA where colorOf _ = mkHSV 100 1 1

instance BaseColor "olive"   RGBA where colorOf _ = mkRGB 0x80 0x80 0x00
instance BaseColor "olive"   HSVA where colorOf _ = mkHSV 60 1 0.502

instance BaseColor "yellow"  RGBA where colorOf _ = mkRGB 0xff 0xff 0x00
instance BaseColor "yellow"  HSVA where colorOf _ = mkHSV 60 1 1

instance BaseColor "navy"    RGBA where colorOf _ = mkRGB 0x00 0x00 0x80
instance BaseColor "navy"    HSVA where colorOf _ = mkHSV 240 1 0.502

instance BaseColor "blue"    RGBA where colorOf _ = mkRGB 0x00 0x00 0xff
instance BaseColor "blue"    HSVA where colorOf _ = mkHSV 240 1 1

instance BaseColor "teal"    RGBA where colorOf _ = mkRGB 0x00 0x00 0x80
instance BaseColor "teal"    HSVA where colorOf _ = mkHSV 180 1 0.502

instance BaseColor "aqua"    RGBA where colorOf _ = mkRGB 0x00 0x00 0xff
instance BaseColor "aqua"    HSVA where colorOf _ = mkHSV 180 1 1

instance BaseColor "orange"  RGBA where colorOf _ = mkRGB 0xff 0x50 0x00
instance BaseColor "orange"  HSVA where colorOf _ = mkHSV 38.8 1 1

instance {-# OVERLAPPABLE #-}
    ( TypeError ( 'Text "No base color `"
            ':<>: 'Text name
            ':<>: 'Text "` defined for format "
            ':<>: 'ShowType format )
    ) => BaseColor name format where
    colorOf _ = error "unreachable"

---------------------
-- Utils functions --
---------------------

convertOn :: (a `ConvertTo` b, b `ConvertTo` a) => (a -> a) -> b -> b
convertOn f = convert . f . convert

convertOn2 :: (a `ConvertTo` b, b `ConvertTo` a) => (a -> a -> a) -> b -> b -> b
convertOn2 f x y = convert $ f (convert x) (convert y)

-- | Degree values (like hue) will roll over if > 360 or < 0
modDegrees :: RealFrac f => f -> f
modDegrees h
    | h < 0   = modDegrees (h + 360)
    | h > 360 = modDegrees (h - 360)
    | otherwise = h