module Verda.Asset.Interop where

import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import qualified Data.Text.Encoding      as T
import qualified Dhall                   as D

import           Verda.Asset.Internal    (simpleEither)
import           Verda.Asset.Types

parseFromDhall :: (D.FromDhall a, MonadIO m) => ByteString -> m (Either String a)
parseFromDhall bs = case T.decodeUtf8' bs of
    Left msg  -> pure $ Left (show msg)
    Right txt ->
        liftIO (try @SomeException (D.input D.auto txt)) >>= \case
            Left e  -> pure $ Left (show e)
            Right v -> pure $ Right v

loadAssetFromDhall :: D.FromDhall a => AssetLoadFn a
loadAssetFromDhall _ bs = simpleEither <$> parseFromDhall bs