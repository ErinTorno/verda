module Verda.AppSpec where

import           Apecs
import           Control.Concurrent.MVar
import           Data.Functor
import qualified Data.Map.Strict         as Map
import           Test.Hspec

import           Verda.App.Internal
import           Verda.Test.Utils

spec :: Spec
spec =
    context "Verda.App.Internal" $ do
        runSystemsTest

data MyState = MyState1 | MyState2 deriving (Eq, Ord, Show)

runSystemsTest :: Spec
runSystemsTest =
    describe "runSystemsTest" $ do
        it "should run all systems for state if no startups" $ do
            sysSum <- newMVar @Int 0
            app <- makeApp (pure ())
               <&> withSystem MyState1 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+1))) >> pure stID)
                 . withSystem MyState1 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+2))) >> pure stID)
                 . withSystem MyState1 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+4))) >> pure stID)
                 . withSystem MyState2 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+8))) >> pure stID)
                 . withInitState MyState1
            (stID, startups) <- runWith () $ runSystems app MyState1 []
            stID `shouldBe` MyState1
            startups `shouldSatisfyNS` null
            readMVar sysSum `shouldReturn` 7
        it "should run no systems for state if startups not finished" $ do
            startSum <- newMVar @Int 0
            sysSum   <- newMVar @Int 0
            app <- makeApp (pure ())
               <&> withSystem  MyState1 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+123))) >> pure stID)
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+1))) >> pure True)
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+2))) >> pure False)
                 . withInitState MyState1
            (stID, startups) <- runWith () $ runSystems app MyState1 (appStartups app Map.! MyState1)
            stID `shouldBe` MyState1
            startups `shouldSatisfyNS` ((==1) . length)
            readMVar startSum `shouldReturn` 3
            readMVar sysSum `shouldReturn` 0
        it "should run all systems if startups finish" $ do 
            startSum <- newMVar @Int 0
            sysSum   <- newMVar @Int 0
            app <- makeApp (pure ())
               <&> withSystem  MyState1 (\stID -> liftIO (modifyMVar_ sysSum (pure . (+1))) >> pure stID)
                 . withStartup MyState1 (liftIO (modifyMVar_ startSum (pure . (+1))) >> pure True)
                 . withInitState MyState1
            (stID, startups) <- runWith () $ runSystems app MyState1 (appStartups app Map.! MyState1)
            stID `shouldBe` MyState1
            startups `shouldSatisfyNS` null
            readMVar startSum `shouldReturn` 1
            readMVar sysSum `shouldReturn` 1