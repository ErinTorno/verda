{-# LANGUAGE TemplateHaskell #-}

module Verda.WorldSpec where

import           Apecs
import           Test.Hspec

import           Verda.World

makeWorld "TestWorld" verdaWorldNames

type SystemT' = SystemT TestWorld IO

spec :: Spec
spec =
    context "Verda.World" $ do
        pure ()