module Verda.App.System where
    
import           Apecs                        hiding (Map)
import           GHC.Generics

data StartupResult = Done | Again deriving (Eq, Generic, Ord, Read, Show)

-- data SystemResult s
--     = ChangeTo s
--     | Continue

type StateStartup w s = s -> SystemT w IO StartupResult

type StateSystem w s = s -> SystemT w IO s

type StateFinalizer w s = s -> SystemT w IO ()

data StateLifetime s
    = ForAnyState
    | ForState s
    deriving (Eq, Ord, Read, Show)