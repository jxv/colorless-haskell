{-# LANGUAGE DataKinds #-}
module Colorless.Endpoint
  ( colorless
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.Async.Lifted ()
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Colorless.Types

colorless
  :: (MonadIO m, FromJSON meta, FromJSON call, ToJSON a, RuntimeThrower m)
  => (Request meta call -> m a)
  -> Value
  -> m Value
colorless handleRequest v = case parseRequest v of
  Nothing -> runtimeThrow RuntimeError'UnparsableFormat
  Just req -> toJSON <$> handleRequest req

parseRequest :: (FromJSON m, FromJSON c) => Value -> Maybe (Request m c)
parseRequest = parseMaybe parseJSON
