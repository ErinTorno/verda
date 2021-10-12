module Verda.Util.Logger where

import           Apecs
import           Control.Monad.IO.Class (MonadIO)
import           Data.Default
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           Say

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
logLine typ msg = get global
              >>= \(Logger f) -> liftIO $ f typ msg

logLineWith :: forall m. MonadIO m => Logger -> LogType -> Text -> m ()
logLineWith (Logger logFn) typ = liftIO . logFn typ

putStrLogger :: Logger
putStrLogger = Logger $ \typ msg ->
    let fullMsg = logTypeText typ <> ": " <> msg <> "\n"
     in if   typ == Error
        then sayErr fullMsg
        else say fullMsg

fileAppendLogger :: FilePath -> Logger
fileAppendLogger filePath = Logger $ \typ msg ->
    T.appendFile filePath (logTypeText typ <> ": " <> msg <> "\n")