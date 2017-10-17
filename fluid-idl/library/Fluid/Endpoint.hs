{-# LANGUAGE DataKinds #-}
module Fluid.Endpoint
  ( runFluid
  , runFluidSingleton
  ) where

import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HML
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except
import Control.Concurrent.Async.Lifted ()
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Data.Map (Map)

import Fluid.Types
import Fluid.Server.Exchange
import Fluid.RuntimeThrower

runFluidSingleton
  :: MonadIO m
  => Version
  -> (Request -> m (Either Response Response))
  -> Value
  -> m Value
runFluidSingleton Version{major,minor} handleRequest = runFluid (Map.singleton major (minor, handleRequest))

runFluid
  :: MonadIO m
  => Map Major (Minor, Request -> m (Either Response Response))
  -> Value
  -> m Value
runFluid handleRequestMap v = do
  e <- runExceptT $ do
    fluidVersion <- getFluidVersion v
    assertFluidVersionCompatiability fluidVersion
    apiVersion <- getApiVersion v
    let apiMajor = major apiVersion
    case Map.lookup apiMajor handleRequestMap of
      Nothing -> case leastAndGreatest (Map.keys handleRequestMap) of
        Nothing -> runtimeThrow RuntimeError'NoImplementation
        Just (minMajor, maxMajor) ->
          if minMajor > apiMajor
            then runtimeThrow RuntimeError'ApiMajorVersionTooLow
            else if maxMajor < apiMajor
              then runtimeThrow RuntimeError'ApiMajorVersionTooHigh
              else runtimeThrow RuntimeError'NoImplementation
      Just (maxMinor, handleRequest) -> if minor apiVersion > maxMinor
        then runtimeThrow RuntimeError'ApiMinorVersionTooHigh
        else case parseRequest v of
          Nothing -> runtimeThrow RuntimeError'UnparsableFormat
          Just req -> toJSON <$> ExceptT (handleRequest req)
  return $ either toJSON id e

leastAndGreatest :: Ord a => [a] -> Maybe (a,a)
leastAndGreatest [] = Nothing
leastAndGreatest xs = Just (minimum xs, maximum xs)

assertFluidVersionCompatiability :: RuntimeThrower m => Version -> m ()
assertFluidVersionCompatiability Version{major,minor}
  | major > mustMajor = runtimeThrow RuntimeError'FluidMajorVersionTooHigh
  | major < mustMajor = runtimeThrow RuntimeError'FluidMajorVersionTooLow
  | minor > maxMinor = runtimeThrow RuntimeError'FluidMinorVersionTooHigh
  | otherwise = return ()
  where
    mustMajor = 0
    maxMinor = 0

getApiVersion :: RuntimeThrower m => Value -> m Version
getApiVersion = getVersion "version" RuntimeError'NoApiVersion

getFluidVersion :: RuntimeThrower m => Value -> m Version
getFluidVersion = getVersion "fluid" RuntimeError'NoFluidVersion

getVersion :: RuntimeThrower m => Text -> RuntimeError -> Value -> m Version
getVersion name err (Object o) = case HML.lookup name o of
  Just x -> maybe (runtimeThrow err) return (parseMaybe parseJSON x)
  Nothing -> runtimeThrow err
getVersion _ err _ = runtimeThrow err

parseRequest :: Value -> Maybe Request
parseRequest = parseMaybe parseJSON
