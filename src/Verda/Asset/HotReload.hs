module Verda.Asset.HotReload where

import           Control.Concurrent
import qualified Data.HashTable.ST.Basic as HT
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST        (stToIO)
import qualified System.FilePath         as FP
import qualified System.FSNotify         as FSNotify

import           Verda.Asset.Path
import           Verda.Asset.Types

-- | If hot reloading is enabled, ensure that a watch is made for this asset now that it has started loading
ensureHotReload :: MonadIO m => Assets -> FSNotify.WatchManager -> Path -> Bool -> m ()
ensureHotReload Assets{..} fsManager path _isMultiThread =
    liftIO $ when (useHotReloading assetSettings) $ do
        let watchDir = addAssetDir (assetFolder assetSettings) (assetDirectory path)
        putStrLn ("ensureHotReload for " <> show path <> "for dir" <> show watchDir)
        stopWatchings <- readMVar assetStopWatchingsByPath
        stToIO (HT.lookup stopWatchings path) >>= \case
            Just _  -> pure ()
            Nothing ->
                let predicate = \case
                        FSNotify.Modified filePath _ _ -> FP.takeFileName filePath == assetFileName path
                        _ -> False
                in do stopWatching <- FSNotify.watchDir fsManager (unPath watchDir) predicate print -- ignore watcher cancel action for now
                      stToIO (HT.insert stopWatchings path stopWatching)