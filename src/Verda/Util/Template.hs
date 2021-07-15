{-# LANGUAGE TemplateHaskell #-}

module Verda.Util.Template where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.IO

embedIO :: IO a -> Q a
embedIO action = runIO $ do
    res <- action
    hFlush stdout -- template haskell does not guarantee compiler flushing these, so we do it now
    hFlush stderr
    pure res

liftWith :: Lift b => (a -> b) -> a -> Q Exp
liftWith f = lift . f

liftMap :: (Lift k, Lift v) => Map k v -> Q Exp
liftMap = liftMapWith lift lift

liftMapWith :: (k -> Q Exp) -> (v -> Q Exp) -> Map k v -> Q Exp
liftMapWith liftKey liftVal m = AppE (VarE 'Map.fromList) <$> mkList (Map.toList m)
    where mkList = fmap ListE
                 . mapM (\(k, v) -> liftKey k >>= \k' -> liftVal v >>= \v'-> pure (TupE [Just k', Just v']))

    