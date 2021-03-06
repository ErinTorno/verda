module Verda.AppSpec where

import           Apecs                   hiding (Map)
import           Control.Concurrent.MVar
import           Data.Functor
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Test.Hspec

import           Verda.App
import           Verda.App.Internal
import           Verda.App.System
import           Verda.Test.Utils
import           Verda.WorldSpec

spec :: Spec
spec =
    context "Verda.App.Internal" $ do
        runSystemsSpec
        withStartupForLifetimeSpec
        withSystemForLifetimeSpec
        withFinalizerForLifetimeSpec
        withDefaultSystemsSpec

data MyState = MyState1 | MyState2 deriving (Eq, Ord, Show)

runSystemsSpec :: Spec
runSystemsSpec =
    context "runSystemsTest" $ do
        it "should run all systems for state if no startups" $ do
            sysSum <- newMVar @Int 0
            app <- makeApp initTestWorld
               <&> withSystemForAll (liftIO (modifyMVar_ sysSum (pure . (+1))) >> pure @SystemT' (Continue @MyState))
                 . withSystem    MyState1 (liftIO (modifyMVar_ sysSum (pure . (+2))) >> pure @SystemT' (Continue @MyState))
                 . withSystem    MyState1 (liftIO (modifyMVar_ sysSum (pure . (+4))) >> pure @SystemT' (Continue @MyState))
                 . withSystem    MyState2 (liftIO (modifyMVar_ sysSum (pure . (+8))) >> pure @SystemT' (Continue @MyState))
                 . withInitState MyState1
            testWorld <- initTestWorld
            (history, startups, systems) <- runWith testWorld $ runSystems app (mkStateHistory MyState1) [] (systemsFor app MyState1)
            peekState history `shouldBe` MyState1
            startups `shouldSatisfyNS` null
            length systems `shouldBe` 3
            readMVar sysSum `shouldReturn` 7
        it "should run no systems for state if startups not finished" $ do
            startSum <- newMVar @Int 0
            sysSum   <- newMVar @Int 0
            app <- makeApp initTestWorld
               <&> withSystem  MyState1 (liftIO (modifyMVar_ sysSum (pure . (+123))) >> pure @SystemT' (Continue @MyState))
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+1))) >> pure @SystemT' Done)
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+2))) >> pure @SystemT' Again)
                 . withInitState MyState1
            testWorld <- initTestWorld
            (history, startups, systems) <- runWith testWorld $ runSystems app (mkStateHistory MyState1)  (startupsFor app MyState1) (systemsFor app MyState1)
            peekState history `shouldBe` MyState1
            length startups `shouldBe` 1
            length systems  `shouldBe` 1
            readMVar startSum `shouldReturn` 3
            readMVar sysSum `shouldReturn` 0
        it "should run all systems if startups finish" $ do 
            startSum <- newMVar @Int 0
            sysSum   <- newMVar @Int 0
            app <- makeApp initTestWorld
               <&> withSystem  MyState1 (liftIO (modifyMVar_ sysSum (pure . (+1))) >> pure @SystemT' (Continue @MyState))
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+1))) >> pure @SystemT' Done)
                 . withInitState MyState1
            testWorld <- initTestWorld
            (history, startups, systems) <- runWith testWorld $ runSystems app (mkStateHistory MyState1)  (startupsFor app MyState1) (systemsFor app MyState1)
            peekState history `shouldBe` MyState1
            length startups `shouldBe` 0
            length systems  `shouldBe` 1
            readMVar startSum `shouldReturn` 1
            readMVar sysSum   `shouldReturn` 1

------------------
-- App Creation --
------------------

withDefaultSystemsSpec :: Spec
withDefaultSystemsSpec =
    context "mkAppWith" $ do
        let counts = Map.map length
        it "should create app with default settings and systems" $ do
            App{..} <- makeApp initTestWorld
                   <&> withDefaultSystems
            counts appStartups   `shouldBe` Map.empty
            counts appSystems    `shouldBe` Map.singleton InBackground 1
            counts appFinalizers `shouldBe` Map.empty
        it "should use newly added and defaults in a chain" $ do
            App{..} <- makeApp initTestWorld
                   <&> withStartup MyState1 (pure @SystemT' Done)
                     . withSystem  MyState2 (pure @SystemT' (Continue @MyState))
                     . withSystemForAll     (pure @SystemT' (Continue @MyState))
                     . withFinalizerForAll  (pure @SystemT' ())
                     . withDefaultSystems
                     . withInitState MyState1
            counts appStartups   `shouldBe` Map.singleton (ForState MyState1) 1
            counts appSystems    `shouldBe` Map.fromList [(ForAnyState, 1), (ForState MyState2, 1), (InBackground, 1)]
            counts appFinalizers `shouldBe` Map.singleton ForAnyState 1

withAnyForLifetimeSpec
    -- :: IsSystem a TestWorld MyState r
    :: String
    -> String
    -> (App TestWorld MyState -> Map (StateLifetime MyState) [SysContext s -> SystemT TestWorld IO r])
    -> a
    -> (StateLifetime MyState -> a -> App TestWorld MyState -> App TestWorld MyState)
    -> Spec
withAnyForLifetimeSpec label name getter dummy withSys =
    context label $ do
        let counts = Map.map length . getter
        it ("should have 1 " <> name <> " for ForAnyState") $ do
            app <- makeApp initTestWorld
               <&> withSys ForAnyState dummy
                 . withInitState MyState1
            counts app `shouldBe`
                Map.fromList [(ForAnyState, 1)]
        it ("should have as many " <> name <> " as its state has when no ForAnyState " <> name) $ do
            app <- makeApp initTestWorld
               <&> withSys (ForState MyState1) dummy
                 . withSys (ForState MyState1) dummy
                 . withInitState MyState1
            counts app `shouldBe`
                Map.fromList [(ForState MyState1, 2)]
        it ("should have sum of both state and ForAnyState " <> name <> "s") $ do
            app <- makeApp initTestWorld
               <&> withSys (ForState MyState1) dummy
                 . withSys (ForState MyState1) dummy
                 . withSys (ForState MyState2) dummy
                 . withSys (ForState MyState2) dummy
                 . withSys ForAnyState         dummy
                 . withSys ForAnyState         dummy
                 . withInitState MyState1
            counts app `shouldBe`
                Map.fromList [(ForState MyState1, 2), (ForState MyState2, 2), (ForAnyState , 2)]

withStartupForLifetimeSpec :: Spec
withStartupForLifetimeSpec = withAnyForLifetimeSpec "withStartupForLifetime" "startup" appStartups (pure @SystemT' ()) withStartupForLifetime

withSystemForLifetimeSpec :: Spec
withSystemForLifetimeSpec = withAnyForLifetimeSpec "withSystemForLifetime" "startup" appSystems (pure @SystemT' ()) withSystemForLifetime

withFinalizerForLifetimeSpec :: Spec
withFinalizerForLifetimeSpec = withAnyForLifetimeSpec "withFinalizerForLifetime" "finalizer" appFinalizers (pure @SystemT' ()) withFinalizerForLifetime