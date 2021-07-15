module Verda.Graphics.ColorSpec where

import           Test.Hspec

import           Verda.Graphics.Color

spec :: Spec
spec =
    context "Verda.Graphics.Color" $ do
        fromHexTest
        toHexTest

fromHexTest :: Spec
fromHexTest =
    describe "fromHex" $ do
        it "should parse css RRGGBB color" $ do
            fromHex "#b0a9e4" `shouldBe` Just (mkRGBA 176 169 228 255)
        it "should parse css RRGGBBAA color" $ do
            fromHex "#dea38b64" `shouldBe` Just (mkRGBA 222 163 139 100)

toHexTest :: Spec
toHexTest =
    describe "toHex" $ do
        it "should write css RRGGBBAA color" $ do
            toHex (mkRGBA 179 227 218 255) `shouldBe` "#b3e3daff"