module Verda.Asset.Path
    ( Path(..)
    , combine
    , addAssetDir
    , addLabel
    , assetDirectory
    , assetExtension
    ) where

import           Data.Char               (toLower)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified System.FilePath.Posix   as FP

import           Verda.Asset.Types

labelSeparator :: Char
labelSeparator = '#'

-- | Combines two paths together
combine :: Path -- ^initial path
        -> Path -- ^ending path; if absolute (starts with /), then the initial path is discarded
        -> Path
combine (Path a) (Path b) = case b of
    '/':_ -> Path $ FP.normalise b
    _     -> Path $ FP.normalise $ FP.combine a b

addAssetDir :: FilePath -> Path -> Path
addAssetDir dir (Path path) = Path $ FP.normalise $ FP.combine dir path

addLabel :: Text -> Path -> Path
addLabel label (Path a) = Path $ concat [a, [labelSeparator], T.unpack label]

assetDirectory :: Path -> Path
assetDirectory (Path p) = Path $ FP.takeDirectory p

assetExtension :: Path -> Text
assetExtension (Path p) =  T.pack . drop 1 . map toLower . takeWhile (/=labelSeparator) . FP.takeExtensions $ p