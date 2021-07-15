{-# LANGUAGE TemplateHaskell #-}

module Verda.Event.Control.Internal where

import           Apecs
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.ST             (RealWorld, stToIO)
import qualified Data.HashTable.ST.Basic      as HT
import           Data.Int
import           Data.Maybe
import qualified Data.Map.Strict              as Map
import           Data.Vector.Unboxed          (Unbox)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed.Mutable  (IOVector)
import qualified Data.Vector.Unboxed.Mutable  as UMVec
import           Data.Word
import qualified Language.Haskell.TH.Syntax   as TH
import           Linear.V2
import qualified SDL
import qualified SDL.Input.GameController     as SDL
import qualified SDL.Internal.Numbered        as SDL

import           Verda.Util.Container         (insideOut)
import           Verda.Util.Template          (embedIO, liftMapWith)

-----------
-- Types --
-----------

type InputState = Word8

pattern BtnUnpressed, BtnPressed, BtnHeld, BtnReleased :: Word8
pattern BtnUnpressed = 0
pattern BtnPressed   = 1
pattern BtnHeld      = 2
pattern BtnReleased  = 3

data JoystickID = JoystickID {joystickIndex :: !Int32, joystickButtonIndex :: !Word8} deriving (Eq, Read, Show)

newtype JoystickHatPos = JoystickHatPos {unJoystickHatPos :: SDL.JoyHatPosition} deriving (Eq, Ord, Read, Show)
instance Enum JoystickHatPos where
    toEnum i = JoystickHatPos $ SDL.fromNumber (fromIntegral i)
    fromEnum (JoystickHatPos pos) = case pos of
        SDL.HatCentered  -> 0
        SDL.HatUp        -> 1
        SDL.HatRight     -> 2
        SDL.HatDown      -> 3
        SDL.HatLeft      -> 4
        SDL.HatRightUp   -> 5
        SDL.HatRightDown -> 6
        SDL.HatLeftUp    -> 7
        SDL.HatLeftDown  -> 8
derivingUnbox "JoystickHatPos"
    [t| JoystickHatPos -> Int |]
    [| fromEnum               |]
    [| toEnum                 |]

-- | A code for reading inputs for all types of SDL input
data InputCode
    = ScanCode             {scanCodeIndex         :: !Int}
    | KeyCode              {keyCodeIndex          :: !Int}
    | MouseButtonCode      {mouseButtonIndex      :: !Int}
    | ControllerButtonCode {controllerButtonIndex :: !Int}
    | ExtraButtonCode      {extraButtonIndex      :: !Int}
    | JoystickCode         {joystickID            :: !JoystickID}
    deriving (Eq, Read, Show)

-- | Holds information about the cursor (usually mouse)
data CursorMotionState = CursorMotionState
    { cursorScreenPosition :: !(V2 Int32)
    , cursorPosition       :: !(V2 Double)
    , cursorMovement       :: !(V2 Double)
    , cursorScrollWheel    :: !Double      -- if scrolling isn't supported, should always be 0
    } deriving (Eq, Read, Show)

instance Semigroup CursorMotionState where (<>) = mappend
instance Monoid CursorMotionState where mempty = CursorMotionState (V2 0 0) (V2 0 0) (V2 0 0) 0
instance Component CursorMotionState where type Storage CursorMotionState = Global CursorMotionState

data JoystickState = JoystickState
    { jAxisStates   :: !(IOVector Int16)
    , jBallStates   :: !(IOVector (V2 Int16))
    , jHatStates    :: !(IOVector JoystickHatPos)
    , jButtonStates :: !(IOVector InputState)
    }

data ControlState = ControlState
    { scanCodeStates         :: !(IOVector InputState)
    , keyCodeStates          :: !(IOVector InputState)
    , mouseButtonStates      :: !(IOVector InputState)
    , controllerButtonStates :: !(IOVector InputState)
    , joystickStates         :: !(HT.HashTable RealWorld Int32 JoystickState)
    , extraMouseButtonStates :: !(HT.HashTable RealWorld Int InputState)
    }

instance Semigroup ControlState where (<>) = mappend
instance Monoid ControlState where mempty = error "ControlState was not initialized before being used; This is a bug and should never happen"
instance Component ControlState where type Storage ControlState = Global ControlState

---------------
-- Functions --
---------------

controllerButtonCount, joystickButtonCount, keyCodeCount, mouseButtonCount, scanCodeCount :: Int
joystickButtonCount = fromIntegral $ maxBound @Word8

isPressedOrHeld :: InputState -> Bool
isPressedOrHeld st = st == BtnPressed || st == BtnHeld

fromScanCode :: SDL.Scancode -> InputCode
fromScanCode (SDL.Scancode w) = ScanCode (fromIntegral w) -- offset to allow special reserved input at the start
scanCodeCount = 512

namesByScancode :: Map.Map SDL.Scancode String 
namesByScancode = $(let rng     = [0..(512 - 1)]
                        liftSC w = [| SDL.Scancode w |]
                        isValid = not . null . snd
                     in embedIO (Map.fromList . filter isValid <$> mapM (\i -> (i,) <$> SDL.getScancodeName (SDL.Scancode i)) rng)
                    >>= liftMapWith liftSC TH.lift)

scancodesByName :: Map.Map String SDL.Scancode
scancodesByName = insideOut namesByScancode

fromKeyCode :: SDL.Keycode  -> InputCode
fromKeyCode (SDL.Keycode w) = ScanCode (fromIntegral w) -- offset to allow special reserved input at the start
keyCodeCount = 512

fromMouseButton :: SDL.MouseButton -> InputCode
fromMouseButton btn = case btn of
    SDL.ButtonLeft    -> MouseButtonCode 1
    SDL.ButtonMiddle  -> MouseButtonCode 2
    SDL.ButtonRight   -> MouseButtonCode 3
    SDL.ButtonX1      -> MouseButtonCode 4
    SDL.ButtonX2      -> MouseButtonCode 5
    SDL.ButtonExtra i -> ExtraButtonCode i
mouseButtonCount = 6

fromControllerButton :: SDL.ControllerButton -> InputCode
fromControllerButton btn = case btn of
    SDL.ControllerButtonInvalid       -> ControllerButtonCode 0
    SDL.ControllerButtonA             -> ControllerButtonCode 1
    SDL.ControllerButtonB             -> ControllerButtonCode 2
    SDL.ControllerButtonX             -> ControllerButtonCode 3
    SDL.ControllerButtonY             -> ControllerButtonCode 4
    SDL.ControllerButtonBack          -> ControllerButtonCode 5
    SDL.ControllerButtonGuide         -> ControllerButtonCode 6
    SDL.ControllerButtonStart         -> ControllerButtonCode 7
    SDL.ControllerButtonLeftStick     -> ControllerButtonCode 8
    SDL.ControllerButtonRightStick    -> ControllerButtonCode 9
    SDL.ControllerButtonLeftShoulder  -> ControllerButtonCode 10
    SDL.ControllerButtonRightShoulder -> ControllerButtonCode 11
    SDL.ControllerButtonDpadUp        -> ControllerButtonCode 12
    SDL.ControllerButtonDpadDown      -> ControllerButtonCode 13
    SDL.ControllerButtonDpadLeft      -> ControllerButtonCode 14
    SDL.ControllerButtonDpadRight     -> ControllerButtonCode 15
controllerButtonCount = 16

mkJoystickState :: MonadIO m => m JoystickState
mkJoystickState = liftIO $ JoystickState
    <$> UMVec.replicate joystickButtonCount 0
    <*> UMVec.replicate joystickButtonCount 0
    <*> UMVec.replicate joystickButtonCount (JoystickHatPos SDL.HatCentered)
    <*> UMVec.replicate joystickButtonCount BtnUnpressed

mkControlState :: MonadIO m => m ControlState
mkControlState = liftIO $ ControlState
    <$> UMVec.replicate scanCodeCount         BtnUnpressed
    <*> UMVec.replicate keyCodeCount          BtnUnpressed
    <*> UMVec.replicate mouseButtonCount      BtnUnpressed
    <*> UMVec.replicate controllerButtonCount BtnUnpressed
    <*> stToIO HT.new
    <*> stToIO HT.new

getInputState :: MonadIO m => ControlState -> InputCode -> m InputState
getInputState ControlState{..} code = liftIO $ case code of
    ScanCode n                         -> safeRead n scanCodeStates
    KeyCode n                          -> safeRead n keyCodeStates
    MouseButtonCode n                  -> safeRead n mouseButtonStates
    ControllerButtonCode n             -> safeRead n controllerButtonStates
    ExtraButtonCode n                  -> fromMaybe BtnUnpressed <$> fromHT n extraMouseButtonStates 
    JoystickCode (JoystickID joyIdx n) -> fromHT joyIdx joystickStates >>= \case
        Nothing                -> pure BtnUnpressed
        Just JoystickState{..} -> safeRead (fromIntegral n) jButtonStates
    where safeRead idx v
              | idx >= 0 && idx < UMVec.length v = UMVec.read v idx
              | otherwise                        = pure BtnUnpressed
          fromHT idx ht = stToIO $ HT.lookup ht idx

tickNextInputFrame :: MonadIO m => ControlState -> m ()
tickNextInputFrame ControlState{..} = liftIO $ do
    let update BtnPressed  = BtnHeld
        update BtnReleased = BtnUnpressed
        update e           = e
    UMVec.mapM_ (pure . update) scanCodeStates
    UMVec.mapM_ (pure . update) mouseButtonStates
    UMVec.mapM_ (pure . update) controllerButtonStates
    stToIO $ HT.mapM_ (\(_, v) -> UMVec.mapM_ (pure . update) (jButtonStates v)) joystickStates
    stToIO $ HT.mapM_ (\(_, v) -> pure (update v)) extraMouseButtonStates

updateInputButton :: MonadIO m => ControlState -> InputCode -> InputState -> m ()
updateInputButton ControlState{..} code newState = liftIO $ case code of
    ScanCode n                         -> safeWrite n scanCodeStates
    KeyCode n                          -> safeWrite n keyCodeStates
    MouseButtonCode n                  -> safeWrite n mouseButtonStates
    ControllerButtonCode n             -> safeWrite n controllerButtonStates
    ExtraButtonCode n                  -> stToIO $ HT.insert extraMouseButtonStates n newState
    JoystickCode (JoystickID joyIdx n) -> stToIO (HT.lookup joystickStates joyIdx) >>= \case
        Just JoystickState{..} -> safeWrite (fromIntegral n) jButtonStates
        Nothing -> do
            st@JoystickState{..} <- mkJoystickState
            safeWrite (fromIntegral n) jButtonStates
            stToIO $ HT.insert joystickStates joyIdx st
    where safeWrite idx v      = inRange idx v $ UMVec.write v idx newState
          inRange idx v action = when (idx >= 0 && idx < UMVec.length v) action

-- Joystick --

updateJoystickInfo :: (MonadIO m, Unbox a) => (JoystickState -> IOVector a) -> ControlState -> JoystickID -> a -> m ()
updateJoystickInfo getVec ControlState{..} (JoystickID joyIdx n) newSt = liftIO (stToIO $ HT.lookup joystickStates joyIdx) >>= \case
    Nothing -> do
        st <- mkJoystickState
        liftIO (stToIO $ HT.insert joystickStates joyIdx st)
        write st
    Just st -> write st
    where write st = let idx = fromIntegral n
                         v   = getVec st
                      in when (idx >= 0 && idx < UMVec.length v) $ liftIO (UMVec.write v idx newSt)

getJoystickInfo :: (MonadIO m, Unbox a) => (JoystickState -> IOVector a) -> ControlState -> JoystickID -> m (Maybe a)
getJoystickInfo getVec ControlState{..} (JoystickID joyIdx n) = liftIO (stToIO $ HT.lookup joystickStates joyIdx) >>= \case
    Nothing -> pure Nothing
    Just st -> let idx = fromIntegral n
                   v   = getVec st
                in if idx >= 0 && idx < UMVec.length v
                   then Just <$> liftIO (UMVec.read v idx)
                   else pure Nothing

updateJoystickAxis :: MonadIO m => ControlState -> JoystickID -> Int16 -> m ()
updateJoystickAxis = updateJoystickInfo jAxisStates

updateJoystickBall :: MonadIO m => ControlState -> JoystickID -> V2 Int16 -> m ()
updateJoystickBall = updateJoystickInfo jBallStates

updateJoystickHat :: MonadIO m => ControlState -> JoystickID -> JoystickHatPos -> m ()
updateJoystickHat = updateJoystickInfo jHatStates

getJoystickAxis :: MonadIO m => ControlState -> JoystickID -> m (Maybe Int16)
getJoystickAxis = getJoystickInfo jAxisStates

getJoystickBall :: MonadIO m => ControlState -> JoystickID -> m (Maybe (V2 Int16))
getJoystickBall = getJoystickInfo jBallStates

getJoystickHat :: MonadIO m => ControlState -> JoystickID -> m (Maybe JoystickHatPos)
getJoystickHat = getJoystickInfo jHatStates
