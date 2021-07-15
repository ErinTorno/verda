{-# LANGUAGE TemplateHaskell #-}

module Verda.Graphics.Color where

import           Data.Vector.Unboxed.Deriving
import           Data.Word
import           Data.Hashable
import           GHC.Generics
import           Linear.V4                    (V4(..))
import           Numeric                      (readHex, showHex)

newtype Color = Color
    { unColor :: V4 Word8 -- ^ RGBA formatted for 0..255
    } deriving (Eq, Ord, Generic, Hashable, Read, Show)
derivingUnbox "Color"
    [t| Color -> V4 Word8 |]
    [| unColor            |]
    [| Color              |]

mkRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkRGBA r g b a = Color $ V4 r g b a

fromHex :: String -> Maybe Color
fromHex ('#':hex) = fromHex hex
fromHex s = case s of
    [r1, r2, g1, g2, b1, b2, a1, a2] -> mkRGBA <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> pair a1 a2
    [r1, r2, g1, g2, b1, b2]         -> mkRGBA <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> Just 255
    _                                -> Nothing
    where pair a b = (\case {[] -> Nothing; ((x, _):_) -> Just x}) . readHex $ [a, b]

toHex :: Color -> String
toHex (Color (V4 r g b a)) = ('#':) . padHex r . padHex g . padHex b . padHex a $ ""
    where padHex n = (++) . pad . showHex n $ ""
          pad s    = let len = length s in if len <= 2 then replicate (2 - len) '0' ++ s else s

black :: Color
black = Color $ V4 0 0 0 255

white :: Color
white = Color $ V4 255 255 255 255