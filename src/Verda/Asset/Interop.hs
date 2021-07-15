module Verda.Asset.Interop where

import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import qualified Data.Text.Encoding      as T
import qualified Dhall                   as D

import           Verda.Asset.Internal    (simpleLoaded)
import           Verda.Asset.Types

parseFromDhall :: (D.FromDhall a, MonadIO m) => ByteString -> m (Either String a)
parseFromDhall bs = case T.decodeUtf8' bs of
    Left msg  -> pure $ Left (show msg)
    Right txt -> do
        val <- liftIO $ D.input D.auto txt
        pure $ Right val

loadAssetFromDhall :: D.FromDhall a => AssetLoadFn a
loadAssetFromDhall _ bs = fmap simpleLoaded
                      <$> parseFromDhall bs