{-# LANGUAGE TemplateHaskell #-}

module Main where

import Apecs
import Control.Monad
import Data.Default

import Verda.App
import Verda.Asset
import Verda.Graphics.Color
import Verda.Graphics.Texture
import Verda.World

-- Components --

data Position = Position Float Float
instance Component Position where type Storage Position = Map Position

data SpecialType = Player | Bullet | Enemy
instance Component SpecialType where type Storage SpecialType = Map SpecialType

data GameState = GameState
     { isInit          :: Bool
     , points          :: Int
     , enemyTex        :: Handle Texture
     , enemyBulletTex  :: Handle Texture
     , playerTex       :: Handle Texture
     , playerBulletTex :: Handle Texture
     , assetSet        :: AssetLoadSet
     } deriving (Eq, Show)
instance Semigroup GameState where (<>) = mappend
instance Monoid    GameState where mempty = error "GameState used before init"
instance Component GameState where type Storage GameState = Global GameState

data StateID = Playing deriving (Eq, Ord, Show)

makeWorld "ExampleWorld" $ verdaWorldNames ++ [''GameState, ''Position, ''SpecialType]

main :: IO ()
main = makeAppWith (def {assetFolder = "examples/assets"}) initExampleWorld >>= start
     . withTitle     "Example Shmup"
     . withStartup   Playing startGameState
     . withSystem    Playing setupGameObjects
     . withInitState Playing
     . withLoaderResource ScaleNearest -- for best scaling of pixel sprites, we use Nearest scaling

-- Systems --

startGameState :: StateID -> SystemT ExampleWorld IO StateID
startGameState stID = do
     assets <- get global
     enemyTex        <- loadHandle assets "enemy.png" 
     enemyBulletTex  <- loadHandle assets "enemy_bullet.png" 
     playerTex       <- loadHandle assets "player.png" 
     playerBulletTex <- loadHandle assets "player_bullet.png" 
     let assetSet = loadSet [awaitHandle enemyTex, awaitHandle enemyBulletTex, awaitHandle playerTex, awaitHandle playerBulletTex]
         isInit   = False
         points   = 0
     global $= GameState{..}
     global $= ClearColor (mkRGBA 40 40 46 0)
     pure stID

setupGameObjects :: StateID -> SystemT ExampleWorld IO StateID
setupGameObjects stID = do
     GameState{..} <- get global
     unless isInit $ do
          assets  <- get global
          summary <- loadSetSummary assets assetSet
          when (isFinished summary) $ do
               modify global $ \gs -> gs {isInit = True}
               case summary of
                    Failed err -> error err -- assets failed to load, so we kill the program with the summary of failed assets
                    _          -> pure ()
     pure stID