{-# LANGUAGE DataKinds #-}
module Colorless.Endpoint
  ( runColorless
  , runColorlessSingleton
  ) where

import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HML
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.Async.Lifted ()
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Data.Map (Map)

import Colorless.Types

runColorlessSingleton
  :: (MonadIO m, RuntimeThrower m)
  => Version
  -> (Request -> m Response)
  -> Value
  -> m Value
runColorlessSingleton Version{major,minor} handleRequest = runColorless (Map.singleton major (minor, handleRequest))

runColorless
  :: (MonadIO m, RuntimeThrower m)
  => Map Major (Minor, Request -> m Response)
  -> Value
  -> m Value
runColorless handleRequestMap v = do
  colorlessVersion <- getColorlessVersion v
  assertColorlessVersionCompatiability colorlessVersion
  apiVersion <- getApiVersion v
  let apiMajor = major apiVersion
  case Map.lookup apiMajor handleRequestMap of
    Nothing -> case leastAndGreatest (Map.keys handleRequestMap) of
      Nothing -> runtimeThrow RuntimeError'NoImplementation
      Just (minMajor, maxMajor) ->
        if minMajor > apiMajor
          then runtimeThrow RuntimeError'ApiVersionTooLow
          else if maxMajor < apiMajor
            then runtimeThrow RuntimeError'ApiVersionTooHigh
            else runtimeThrow RuntimeError'NoImplementation
    Just (minMinor, handleRequest) -> if minor apiVersion < minMinor
      then runtimeThrow RuntimeError'ApiVersionTooLow
      else case parseRequest v of
        Nothing -> runtimeThrow RuntimeError'UnparsableFormat
        Just req -> toJSON <$> handleRequest req

leastAndGreatest :: Ord a => [a] -> Maybe (a,a)
leastAndGreatest [] = Nothing
leastAndGreatest xs = Just (minimum xs, maximum xs)

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

parseRequest :: Value -> Maybe Request
parseRequest = parseMaybe parseJSON
