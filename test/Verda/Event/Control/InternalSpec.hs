module Verda.Event.Control.InternalSpec where

import qualified Data.Map.Strict              as Map
import           Test.Hspec

import           Verda.Event.Control.Internal

spec :: Spec
spec =
    context "Verda.Event.Control.Internal" $ do
        getInputStateTest
        getJoystickAxisTest
        namesByScancodeTest
        updateInputButtonTest
        updateJoystickAxisTest

getInputStateTest :: Spec 
getInputStateTest =
    describe "getInputState" $ do
        let sc = ScanCode 0
        it "should return BtnUnpressed for new ControlState" $ do
            st <- mkControlState
            getInputState st sc `shouldReturn` BtnUnpressed

getJoystickAxisTest :: Spec 
getJoystickAxisTest =
    describe "getJoystickAxis" $ do
        it "should be 0 for never modified (but valid) joystick axis" $ do
            st <- mkControlState 
            getJoystickAxis st (JoystickID 0 0) `shouldReturn` Nothing
        it "should be Nothing for invalid joystick axis" $ do
            st <- mkControlState 
            getJoystickAxis st (JoystickID 1234567 0) `shouldReturn` Nothing

namesByScancodeTest :: Spec 
namesByScancodeTest =
    describe "namesByScancode" $ do
        it "should have a reasonable amount of pairs (at least 200)" $
            Map.size namesByScancode `shouldSatisfy` (>200)

updateInputButtonTest :: Spec
updateInputButtonTest =
    describe "updateInputButton" $ do
        let pressAndExpect code = do
                st <- mkControlState
                updateInputButton st code BtnPressed
                getInputState st code `shouldReturn` BtnPressed
        it "should set ScanCode state" $
            pressAndExpect $ ScanCode 0
        it "should set KeyCode state" $
            pressAndExpect $ KeyCode 0
        it "should set MouseButtonCode state" $
            pressAndExpect $ MouseButtonCode 0
        it "should set ExtraButtonCode state" $
            pressAndExpect $ ExtraButtonCode 0
        it "should set JoystickCode btuuon state" $
            pressAndExpect $ JoystickCode (JoystickID 0 0)

updateJoystickAxisTest :: Spec 
updateJoystickAxisTest =
    describe "updateJoystickAxis" $ do
        let jid = JoystickID 0 0
        it "should set axis to given int" $ do
            st <- mkControlState 
            updateJoystickAxis st jid 123
            getJoystickAxis st jid `shouldReturn` Just 123