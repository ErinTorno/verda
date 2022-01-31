module Verda.Util.ContainerSpec where

import qualified Data.Map.Strict              as Map
import           Test.Hspec

import           Verda.Util.Container

spec :: Spec
spec =
    context "Verda.Util.Container" $ do
        insideOutSpec

insideOutSpec :: Spec 
insideOutSpec =
    context "insideOut" $ do
        it "should return an empty map when given" $
            insideOut (Map.empty @Int @Char) `shouldBe` Map.empty
        it "should return a map of values to keys" $
            let input    = Map.fromList @Int @Char [(1, 'a'), (2, 'b')]
                expected = Map.fromList @Char @Int [('a', 1), ('b', 2)]
             in insideOut input `shouldBe` expected