{-# LANGUAGE TemplateHaskell #-}

module Main where

import Apecs
import Data.Default
import qualified SDL.Input.Keyboard.Codes as SDL

import Verda.App
import Verda.Asset
import Verda.Event.Control
import Verda.Graphics.Color
import Verda.Graphics.Texture
import Verda.Util.Error        (sayErrStringAndExit)
import Verda.Util.Logger
import Verda.World

-- Components --

data Position = Position Float Float
instance Component Position where type Storage Position = Map Position

data Player = Player
instance Component Player where type Storage Player = Unique Player
data Bullet = Bullet
instance Component Bullet where type Storage Bullet = Map Bullet
data Enemy = Enemy
instance Component Enemy where type Storage Enemy = Map Enemy

data GameState = GameState
    { points          :: Int
    , enemyTex        :: Handle Texture
    , enemyBulletTex  :: Handle Texture
    , playerTex       :: Handle Texture
    , playerBulletTex :: Handle Texture
    , assetSet        :: AssetLoadSet
    } deriving (Eq, Show)
instance Semigroup GameState where (<>) = mappend
instance Monoid    GameState where mempty = error "GameState used before init"
instance Component GameState where type Storage GameState = Global GameState

-- Run

data StateID = Playing deriving (Eq, Ord, Show)

makeWorld "ExampleWorld" $ verdaWorldNames ++ [''GameState, ''Position, ''Bullet, ''Enemy, ''Player]

type SystemT' = SystemT ExampleWorld IO

main :: IO ()
main = makeAppWith (def {assetFolder = "examples/assets", useHotReloading = True}) initExampleWorld >>= start
     . withTitle     "Example Shmup"
     . withStartup   Playing playingLoadAssetsStartUp
     . withStartup   Playing playingPlaceEtyStartUp
     . withSystem    Playing handleInput
     . withInitState Playing
     . withLoaderResource ScaleNearest -- for best scaling of pixel sprites, we use Nearest scaling

-- Systems --

playerSpeedPPS :: Float
playerSpeedPPS = 200

playingLoadAssetsStartUp :: SystemT' Bool
playingLoadAssetsStartUp = do
     assets <- get global
     enemyTex        <- loadHandle assets "enemy.png" 
     enemyBulletTex  <- loadHandle assets "enemy_bullet.png" 
     playerTex       <- loadHandle assets "player.png" 
     playerBulletTex <- loadHandle assets "player_bullet.png" 
     let assetSet = loadSet [awaitHandle enemyTex, awaitHandle enemyBulletTex, awaitHandle playerTex, awaitHandle playerBulletTex]
         points   = 0
     global $= GameState{..}
     global $= ClearColor (mkRGBA 40 40 46 0)
     pure True

playingPlaceEtyStartUp :: SystemT' Bool
playingPlaceEtyStartUp = do
     GameState{..} <- get global
     assets  <- get global
     summary <- loadSetSummary assets assetSet
     if   isFinished summary
     then case summary of
               Failed err -> sayErrStringAndExit err -- assets failed to load, so we kill the program with the summary of failed assets
               _          -> do
                    logLine Info "All assets finished!"
                    _ety <- newEntity (Player, Position 300 600)
                    pure True
     else pure False

handleInput :: StateID -> SystemT' StateID
handleInput stID = do
     Time dt _ <- get global
     input     <- get global
     let ifUsed code action = getInputState input (fromScanCode code) >>= \inputSt ->
             if isPressedOrHeld inputSt then action else pure ()
     ifUsed SDL.ScancodeW $
          cmap $ \(Player, Position x y) -> Position x (y - playerSpeedPPS * dt)
     ifUsed SDL.ScancodeA $
          cmap $ \(Player, Position x y) -> Position (x - playerSpeedPPS * dt) y
     ifUsed SDL.ScancodeS $
          cmap $ \(Player, Position x y) -> Position x (y + playerSpeedPPS * dt)
     ifUsed SDL.ScancodeD $
          cmap $ \(Player, Position x y) -> Position (x + playerSpeedPPS * dt) y
     pure stID