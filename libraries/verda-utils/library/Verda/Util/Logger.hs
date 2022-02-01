module Verda.Util.Logger where

import           Apecs
import           Control.Monad.IO.Class (MonadIO)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format
import           Data.Default
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Exit            (exitFailure)

data LogType = Info | Warning | Error deriving (Eq, Ord, Read, Show)

newtype Logger = Logger {unLogger :: LogType -> Text -> IO ()}

instance Default Logger where def = putStrLogger
instance Semigroup Logger where (<>) = mappend
instance Monoid Logger where mempty  = def
instance Component Logger where type Storage Logger = Global Logger

logTypeText :: LogType -> Text
logTypeText Info    = "Info"
logTypeText Warning = "Warning"
logTypeText Error   = "Error"

logLine :: forall w m. (MonadIO m, Get w m Logger) => LogType -> Text -> SystemT w m ()
logLine typ msg = get global >>= \l -> logLineWith l typ msg

logString :: forall w m. (MonadIO m, Get w m Logger) => LogType -> String -> SystemT w m ()
logString typ msg = get global >>= \l -> logLineWith l typ (T.pack msg)

logLineWith :: forall m. MonadIO m => Logger -> LogType -> Text -> m ()
logLineWith (Logger logFn) typ = liftIO . logFn typ

logStringWith :: forall m. MonadIO m => Logger -> LogType -> String -> m ()
logStringWith (Logger logFn) typ = liftIO . logFn typ . T.pack

logAndExit :: forall w m a. (MonadIO m, Get w m Logger) => LogType -> Text -> SystemT w m a
logAndExit typ msg = get global >>= \l -> logAndExitWith l typ msg

logStringAndExit :: forall w m a. (MonadIO m, Get w m Logger) => LogType -> String -> SystemT w m a
logStringAndExit typ msg = get global >>= \l -> logStringAndExitWith l typ msg

logAndExitWith :: forall m a. MonadIO m => Logger -> LogType -> Text -> m a
logAndExitWith logger typ msg = do
    logLineWith logger typ msg
    liftIO exitFailure

logStringAndExitWith :: forall m a. MonadIO m => Logger -> LogType -> String -> m a
logStringAndExitWith logger typ = logAndExitWith logger typ . T.pack

putStrLogger :: Logger
putStrLogger = Logger $ \typ msg -> do
    time <- getCurrentTime
    let fullMsg = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%7q " time)
               <> logTypeText typ
               <> ": "
               <> msg
    T.putStrLn fullMsg

fileAppendLogger :: FilePath -> Logger
fileAppendLogger filePath = Logger $ \typ msg ->
    T.appendFile filePath (logTypeText typ <> ": " <> msg <> "\n")