module Verda.Util.Error where

import           Control.Monad.IO.Class       (MonadIO(..))
import           Data.Text                    (Text)
import           Say                          (sayErr, sayErrString)
import           System.Exit                  (exitFailure)

sayErrAndExit :: MonadIO m => Text -> m a
sayErrAndExit msg = do
    sayErr msg
    liftIO exitFailure

sayErrStringAndExit :: MonadIO m => String -> m a
sayErrStringAndExit msg = do
    sayErrString msg
    liftIO exitFailure