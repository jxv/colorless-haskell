{-# OPTIONS_GHC -fno-warn-orphans #-}
module Colorless.ServiceThrower
  ( ServiceThrower(..)
  , ThrownValue(..)
  ) where

import Control.Exception.Safe
import Control.Monad.Except
import Data.Aeson

import qualified Colorless.Server.Exchange as Server

newtype ThrownValue = ThrownValue { unThrownValue :: Value }
  deriving (Show, Eq, Typeable)

class MonadThrow m => ServiceThrower m where
  serviceThrow :: ThrownValue -> m a
  serviceThrow err = throw err

instance Exception ThrownValue

instance MonadThrow m => ServiceThrower (ExceptT Server.Response m) where
  serviceThrow = throwError . Server.Response'Error . Server.ResponseError'Service . unThrownValue
