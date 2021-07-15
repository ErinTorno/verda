{-# LANGUAGE TemplateHaskell #-}

module Verda.World
   ( VerdaWorld
   , ShouldQuit(..)
   , Time(..)
   , verdaWorldNames
   -- re-export
   , module Verda.Data.Components
   , module Verda.Event.Components
   , module Verda.Graphics.Components
   ) where

import           Language.Haskell.TH.Syntax   (Name)

import           Verda.Asset
import           Verda.Data.Components
import           Verda.Event.Components
import           Verda.Graphics.Components
import           Verda.Util.Apecs

type VerdaWorld w m = ReadWriteEach w m
    -- Verda.Asset
   '[ Assets
    -- Verda.Data
    , ShouldQuit, Time
    -- Verda.Event
    , ControlState, CursorMotionState
    -- Verda.Graphics
    , ClearColor, RenderPosition, Tint
    ]

verdaWorldNames :: [Name]
verdaWorldNames =
   -- Verda.Asset
   [ ''Assets
   -- Verda.Data
   , ''ShouldQuit, ''Time
   -- Verda.Event
   , ''ControlState, ''CursorMotionState
   -- Verda.Graphics
   , ''ClearColor, ''RenderPosition, ''Tint
   ]