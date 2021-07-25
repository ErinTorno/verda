module Verda.Test.Utils where

import           Control.Monad
import qualified Data.Foldable           as F
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
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

---------
-- Map --
---------

shouldHaveMember :: (HasCallStack, Ord k, Show k, Show a) => Map k a -> k -> Expectation
shouldHaveMember m k = unless (Map.member k m) (HUnit.assertFailure msg)
    where msg = concat ["no member ", show k, ": ", show m]

shouldHaveMemberNS :: (HasCallStack, Ord k, Show k) => Map k a -> k -> Expectation
shouldHaveMemberNS m k = unless (Map.member k m) (HUnit.assertFailure msg)
    where msg = "no member " ++ show k

shouldHaveMembers :: (HasCallStack, Foldable t, Ord k, Show k, Show a) => Map k a -> t k -> Expectation
shouldHaveMembers m keys = unless (expectedSet `Set.isSubsetOf` mapKeys) (HUnit.assertFailure msg)
    where msg         = concat ["no members ", show expectedSet, ": ", show m]
          expectedSet = Set.fromList $ F.toList keys
          mapKeys     = Set.fromList $ Map.keys m

shouldHaveMembersNS :: (HasCallStack, Foldable t, Ord k, Show k) => Map k a -> t k -> Expectation
shouldHaveMembersNS m keys = unless (expectedSet `Set.isSubsetOf` mapKeys) (HUnit.assertFailure msg)
    where msg         = concat ["no members ", show expectedSet, " in keys: ", show mapKeys]
          expectedSet = Set.fromList $ F.toList keys
          mapKeys     = Set.fromList $ Map.keys m

-----------
-- Dhall --
-----------

shouldDecodeTo :: (D.FromDhall a, Eq a, HasCallStack, Show a) => Text -> a -> Expectation
shouldDecodeTo txt def = D.input D.auto txt `shouldReturn` def