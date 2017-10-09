module Colorless.ServiceThrower
  ( ServiceThrower(..)
  ) where

import Control.Monad.Except
import Data.Aeson

import qualified Colorless.Server.Exchange as Server

class Monad m => ServiceThrower m where
  serviceThrow :: Value -> m a

instance ServiceThrower IO where
  serviceThrow err = error $ "Service error - " ++ show err

instance Monad m => ServiceThrower (ExceptT Server.Response m) where
  serviceThrow = throwError . Server.Response'Error . Server.ResponseError'Service
