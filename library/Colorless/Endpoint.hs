{-# LANGUAGE DataKinds #-}
module Colorless.Endpoint
  ( runColorless
  ) where

import qualified Data.HashMap.Lazy as HML
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.Async.Lifted ()
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)

import Colorless.Types

runColorless
  :: (MonadIO m, FromJSON meta, FromJSON call, ToJSON a, RuntimeThrower m)
  => (Request meta call -> m a)
  -> Value
  -> m Value
runColorless handleRequest v = do
  colorlessVersion <- getColorlessVersion v
  assertColorlessVersionCompatiability colorlessVersion
  _apiVersion <- getApiVersion v
  case parseRequest v of
    Nothing -> runtimeThrow RuntimeError'UnparsableFormat
    Just req -> toJSON <$> handleRequest req

assertColorlessVersionCompatiability :: RuntimeThrower m => Version -> m ()
assertColorlessVersionCompatiability Version{major,minor}
  | major > mustMajor = runtimeThrow RuntimeError'ColorlessVersionTooHigh
  | major < mustMajor = runtimeThrow RuntimeError'ColorlessVersionTooLow
  | minor > maxMinor = runtimeThrow RuntimeError'ColorlessVersionTooHigh
  | minor < minMinor = runtimeThrow RuntimeError'ColorlessVersionTooLow
  | otherwise = return ()
  where
    mustMajor = 0
    minMinor = 0
    maxMinor = 0

getApiVersion :: RuntimeThrower m => Value -> m Version
getApiVersion = getVersion "version" RuntimeError'NoApiVersion

getColorlessVersion :: RuntimeThrower m => Value -> m Version
getColorlessVersion = getVersion "colorless" RuntimeError'NoColorlessVersion

getVersion :: RuntimeThrower m => Text -> RuntimeError -> Value -> m Version
getVersion name err (Object o) = case HML.lookup name o of
  Just x -> maybe (runtimeThrow err) return (parseMaybe parseJSON x)
  Nothing -> runtimeThrow err
getVersion _ err _ = runtimeThrow err

parseRequest :: (FromJSON m, FromJSON c) => Value -> Maybe (Request m c)
parseRequest = parseMaybe parseJSON
