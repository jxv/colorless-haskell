module Colorless.RuntimeThrower
  ( RuntimeThrower(..)
  ) where

import Control.Monad.Except

import qualified Colorless.Server.Exchange as Server
import Colorless.Types

class Monad m => RuntimeThrower m where
  runtimeThrow :: RuntimeError -> m a

instance RuntimeThrower IO where
  runtimeThrow err = error $ "Runtime error - " ++ show err

instance Monad m => RuntimeThrower (ExceptT Server.Response m) where
  runtimeThrow = throwError . Server.Response'Error . Server.ResponseError'Runtime
