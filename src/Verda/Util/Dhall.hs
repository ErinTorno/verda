module Verda.Util.Dhall where

import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Manipulate         as T
import qualified Dhall                        as D

stripConPrefix :: Text -> D.InterpretOptions -> D.InterpretOptions
stripConPrefix prefix options = options {D.constructorModifier = \n -> fromMaybe n $ T.stripPrefix prefix n}

stripFieldPrefix :: Text -> D.InterpretOptions -> D.InterpretOptions
stripFieldPrefix prefix options = options {D.fieldModifier = \n -> T.toCamel $ fromMaybe n $ T.stripPrefix prefix n}