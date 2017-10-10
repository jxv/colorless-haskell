{-# OPTIONS_GHC -fno-warn-orphans #-}
module Colorless.ServiceThrower
  ( ServiceThrower(..)
  ) where

import Control.Exception.Safe
import Control.Monad.Except
import Data.Aeson

import qualified Colorless.Server.Exchange as Server

class MonadThrow m => ServiceThrower m where
  serviceThrow :: Value -> m a
  serviceThrow err = throw err

instance Exception Value

instance MonadThrow m => ServiceThrower (ExceptT Server.Response m) where
  serviceThrow = throwError . Server.Response'Error . Server.ResponseError'Service
