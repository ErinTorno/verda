module Verda.Event.Control
    ( ControlState
    , CursorMotionState(..)
    , InputCode(..)
    , InputState
    , JoystickID(..)
    , JoystickHatPos(..)
    , JoystickState(..)
    -- patterns
    , pattern BtnUnpressed
    , pattern BtnPressed
    , pattern BtnHeld
    , pattern BtnReleased
    -- functions and values
    , fromControllerButton
    , fromJoystickID
    , fromKeyCode
    , fromMouseButton
    , fromScanCode
    , getInputState
    , getJoystickAxis
    , getJoystickBall
    , getJoystickHat
    , isPressedOrHeld
    ) where

import Verda.Event.Control.Internal