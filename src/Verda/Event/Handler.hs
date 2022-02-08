module Verda.Event.Handler (handleEvents) where

import           Apecs
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Default
import           Data.Maybe                   (fromMaybe)
import           Linear.V2
import qualified SDL

import           Verda.Event.Control.Internal
import           Verda.Graphics.Camera        (pixelToScreenRatioScale, pixelToWorldCoord)
import           Verda.Graphics.Components
import           Verda.Util.Apecs
import           Verda.World

handleEvents :: (MonadIO m, VerdaWorld w m) => SystemT w m ()
handleEvents = do
    controlSt       <- get global
    cursorMotion    <- get global
    winResolution   <- get global

    stepNextInputFrame controlSt
    -- set motion to zero now
    global       $= cursorMotion {cursorMovement = V2 0 0, cursorScrollWheel = 0}

    (RenderPosition camPos, camera) <- fromMaybe (def, def) <$> getUnique

    let scale          = pixelToScreenRatioScale camera winResolution

    SDL.mapEvents $ handleEvent controlSt scale
    -- sets the cursor position to a world-adjusted coordinate from the current pixel it is at
    -- every frame, so even if mouse position doesn't actually change, this will be up to date for the current window resolution and camera size
    modify global $ \motion -> motion {cursorPosition = pixelToWorldCoord camera camPos winResolution (cursorScreenPosition motion)}

handleEvent :: (MonadIO m, VerdaWorld w m) => ControlState -> V2 Float -> SDL.Event -> SystemT w m ()
handleEvent controlSt scale event =
    case SDL.eventPayload event of
        SDL.QuitEvent -> global $= ShouldQuit True
        -- marks key with its state
        SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ keysym) ->
            let key = fromScanCode $ SDL.keysymScancode keysym
            in updateInputButton controlSt key (inputSt motion)
        -- sets mouse clicks
        SDL.MouseButtonEvent SDL.MouseButtonEventData{SDL.mouseButtonEventMotion = motion, SDL.mouseButtonEventButton = btn} ->
            updateInputButton controlSt (fromMouseButton btn) (inputSt motion)
        -- sets mouse's position and motion
        SDL.MouseMotionEvent SDL.MouseMotionEventData{SDL.mouseMotionEventPos = SDL.P pos, SDL.mouseMotionEventRelMotion = relPos} -> do
            motionState <- get global
            global      $= motionState { cursorScreenPosition = pos
                                       , cursorPosition       = scale * (fromIntegral <$> pos)
                                       , cursorMovement       = scale * (fromIntegral <$> relPos) }
        -- sets mouse's scrolling amount
        SDL.MouseWheelEvent SDL.MouseWheelEventData{SDL.mouseWheelEventPos = V2 _ y, SDL.mouseWheelEventDirection = dir} -> do
            motionState <- get global
            global      $= motionState {cursorScrollWheel = fromIntegral y * (if dir == SDL.ScrollFlipped then -1 else 1)}
        _ -> pure ()
    where inputSt SDL.Released = BtnUnpressed
          inputSt SDL.Pressed  = BtnUnpressed