module Verda.Graphics.ColorSpec where

import           Test.Hspec

import           Verda.Graphics.Color

spec :: Spec
spec =
    context "Verda.Graphics.Color" $ do
        convertRGBAtoHSVA
        convertHSVAtoRGBA
        fromHexSpec
        modDegreesSpec
        toHexSpec

convertRGBAtoHSVA :: Spec
convertRGBAtoHSVA =
    context "convert @RGBA @HSVA" $ do
        it "should convert black" $ do
            convert (mkRGB 0 0 0) `shouldBe` mkHSV 0 0 0 
        it "should convert white" $ do
            convert (mkRGB 255 255 255) `shouldBe` mkHSV 0 0 1
        it "should convert cornflower blue" $ do
            let HSVA h s v a = convert $ mkRGB 100 149 237
                roundF = fromIntegral @Int @_ . round
                r2dec = (/100) . roundF . (*100)
             in HSVA (roundF h) (r2dec s) (r2dec v) a `shouldBe` mkHSV 219 0.58 0.93

convertHSVAtoRGBA :: Spec
convertHSVAtoRGBA =
    context "convert @HSVA @RGBA" $ do
        it "should convert black" $ do
            convert (mkHSV 0 0 0) `shouldBe` mkRGB 0 0 0
        it "should convert white" $ do
            convert (mkHSV 0 0 1) `shouldBe` mkRGB 255 255 255
        it "should convert cornflower blue" $ do
            convert (mkHSV 219 0.58 0.93) `shouldBe` mkRGB 100 148 237

fromHexSpec :: Spec
fromHexSpec =
    context "fromHex" $ do
        it "should parse css RRGGBB color" $ do
            fromHex "#b0a9e4" `shouldBe` Just (mkRGBA 176 169 228 255)
        it "should parse css RRGGBBAA color" $ do
            fromHex "#dea38b64" `shouldBe` Just (mkRGBA 222 163 139 100)

modDegreesSpec :: Spec
modDegreesSpec =
    context "modDegrees" $ do
        it "should be identity of 0" $
            modDegrees @Double 0 `shouldBe` 0
        it "should be identity of 360" $
            modDegrees @Double 360 `shouldBe` 360
        it "should be identity of in range number" $
            modDegrees @Double 180 `shouldBe` 180
        it "should mod < 0 number" $
            modDegrees @Double -90 `shouldBe` 270
        it "should mod > 360 number" $
            modDegrees @Double 450 `shouldBe` 90
        it "should mod number many leaps away" $
            modDegrees @Double 3645 `shouldBe` 45

toHexSpec :: Spec
toHexSpec =
    context "toHex" $ do
        it "should write css RRGGBBAA color" $ do
            toHex (mkRGBA 179 227 218 255) `shouldBe` "#b3e3daff"