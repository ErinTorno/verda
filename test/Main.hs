module Main where

import           Test.Hspec.Runner
import qualified Spec

main :: IO ()
main = hspecWith defaultConfig Spec.spec
    -- hspecWith (defaultConfig {configFailureReport = Just "test_summary.txt"})
    -- summary <- hspecWithResult defaultConfig Spec.spec
    -- writeFile "test_summary.txt" $ show summary
    -- evaluateSummary summary