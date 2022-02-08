module Verda.App.System where
    
import           Apecs                        hiding (Map)
import           Data.Default
import qualified Data.List.NonEmpty           as NE
import           GHC.Generics

import           Verda.Util.Types

data StartupResult = Done | Again deriving (Eq, Generic, Ord, Read, Show)

instance () `ConvertTo` StartupResult where
    convert _ = Done

data SysResult s
    = Continue
    | RemoveThis
    | PushState s
    | PopState
    deriving (Eq, Generic, Ord, Read, Show)

instance () `ConvertTo` SysResult s where
    convert _ = Continue

data StateLifetime s
    = ForAnyState
    | ForState s
    | InBackground
    deriving (Eq, Generic, Ord, Read, Show)

newtype SysContext s = SysContext {currentState :: s} deriving (Eq, Generic, Ord, Read, Show)

newtype StateHistory s = StateHistory {unStateHistory :: NE.NonEmpty s} deriving (Eq, Generic, Ord, Read, Show)

instance Default s => Default (StateHistory s) where
    def = StateHistory (def NE.:| [])

mkStateHistory :: s -> StateHistory s
mkStateHistory defSt = StateHistory (defSt NE.:| [])

pushState :: Eq s => s -> StateHistory s -> StateHistory s
pushState s h@(StateHistory ne@(cur NE.:| _))
    | s == cur = h
    | otherwise = StateHistory $ NE.cons s ne

popState :: StateHistory s -> (s, StateHistory s)
popState sh@(StateHistory ne) = case ne of
    h NE.:| []     -> (h, sh)
    h NE.:| (s:ss) -> (h, StateHistory (s NE.:| ss))

popState_ :: StateHistory s -> StateHistory s
popState_ = snd . popState

peekState :: StateHistory s -> s
peekState (StateHistory (s NE.:| _)) = s

class IsSystem a w s r where
    asStateSystem :: a -> SysContext s -> SystemT w IO r

instance (r `ConvertTo` r2, w1 ~ w2) => IsSystem (SystemT w1 IO r) w2 s r2 where
    asStateSystem action = \_ -> convert <$> action

instance (IsSystem a w s r) => IsSystem (SysContext s -> a) w s r where
    asStateSystem action = \s -> asStateSystem (action s) s