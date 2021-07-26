module Verda.System.Renderer where

import           Apecs
import           Control.Monad.IO.Class
import qualified SDL

import           Verda.Graphics.Color      (Color(..))
import           Verda.Graphics.Components
import           Verda.World

rendererSystem :: (MonadIO m, VerdaWorld w m) => SDL.Renderer -> SystemT w m ()
rendererSystem renderer = do
    Time tick elapsed           <- get global
    RenderTime (Time renTick _) <- get global
    TargetRefreshRate fps       <- get global
    let delay   = if fps <= 1.0 then 1.0 else 1.0 / fps
        newTick = tick + renTick
    if newTick < delay
    then do
        global $= RenderTime (Time newTick elapsed)
    else do
        -- liftIO $ putStrLn $ "Drawing! delay " ++ show delay ++ "; newTick " ++ show newTick
        ClearColor (Color bkgV4) <- get global 
        SDL.rendererDrawColor renderer SDL.$= bkgV4
        SDL.clear renderer
        SDL.present renderer
        global $= RenderTime (Time (newTick - delay) elapsed)