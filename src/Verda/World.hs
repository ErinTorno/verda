{-# LANGUAGE TemplateHaskell #-}

module Verda.World
   ( VerdaWorld
   , verdaWorldNames
   -- re-export
   , module Verda.Component.Entity
   , module Verda.Component.Global
   ) where

import           Language.Haskell.TH.Syntax   (Name)

import           Verda.Asset
import           Verda.Component.Entity
import           Verda.Component.Global
import           Verda.Util.Apecs

type VerdaWorld w m = ReadWriteEach w m
   '[ ClearColor, ShouldQuit, Time
    , Assets, RenderPosition, Tint
    ]

verdaWorldNames :: [Name]
verdaWorldNames =
   [ ''ClearColor, ''ShouldQuit, ''Time
   , ''Assets, ''RenderPosition, ''Tint
   ]