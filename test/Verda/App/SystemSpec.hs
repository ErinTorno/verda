module Verda.App.SystemSpec where

import           Apecs
import           Test.Hspec

import           Verda.App.System
import           Verda.Test.Utils
import           Verda.WorldSpec

spec :: Spec
spec =
    context "Verda.App.System" $ do
        asStateSystemSpec

data State = State1

asStateSystemSpec :: Spec
asStateSystemSpec =
    context "asStateSystem" $ do
        let asSys :: IsSystem a TestWorld State r => proxy r -> a -> SysContext State -> SystemT TestWorld IO r
            asSys _ = asStateSystem
            compiles proxy a = asSys proxy a `shouldSatisfyNS` const True
        it "should accept SystemT TestWorld IO ()" $ do
            let sys :: SystemT TestWorld IO ()
                sys = pure ()
            compiles (Proxy @()) sys
        it "should accept SystemT TestWorld IO StartupResult" $ do
            let sys :: SystemT TestWorld IO StartupResult
                sys = pure Again
            compiles (Proxy @StartupResult) sys
        it "should accept SystemT TestWorld IO (SysResult State)" $ do
            let sys :: SystemT TestWorld IO (SysResult State)
                sys = pure Continue
            compiles (Proxy @(SysResult State)) sys
        it "should accept SysContext State -> SystemT TestWorld IO ()" $ do
            let sys :: SysContext State -> SystemT TestWorld IO ()
                sys _ = pure ()
            compiles (Proxy @()) sys
        it "should accept SysContext State -> SystemT TestWorld IO StartupResult" $ do
            let sys :: SysContext State -> SystemT TestWorld IO StartupResult
                sys _ = pure Again
            compiles (Proxy @StartupResult) sys
        it "should accept SysContext State -> SystemT TestWorld IO (SysResult State)" $ do
            let sys :: SysContext State -> SystemT TestWorld IO (SysResult State)
                sys _ = pure Continue
            compiles (Proxy @(SysResult State)) sys