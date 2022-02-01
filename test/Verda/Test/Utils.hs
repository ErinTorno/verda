module Verda.Test.Utils where

import           Control.Monad
import           Data.Text               (Text)
import qualified Dhall                   as D
import           Test.Hspec
import qualified Test.HUnit              as HUnit

failWith :: HasCallStack => String -> Expectation
failWith = HUnit.assertFailure 

------------
-- Assets --
------------

shouldSatisfyNS :: HasCallStack => a -> (a -> Bool) -> Expectation
shouldSatisfyNS v p = unless (p v) $ HUnit.assertFailure msg
    where msg = "predicate failed"

-----------
-- Dhall --
-----------

shouldDecodeTo :: (D.FromDhall a, Eq a, HasCallStack, Show a) => Text -> a -> Expectation
shouldDecodeTo txt def = D.input D.auto txt `shouldReturn` def