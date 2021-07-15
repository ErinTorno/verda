module Main where

import           Test.Hspec.Runner
import qualified Spec

main :: IO ()
main = hspecWith (defaultConfig {configFailureReport = Just "test_summary.txt"}) Spec.spec