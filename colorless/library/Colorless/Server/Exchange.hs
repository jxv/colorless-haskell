module Colorless.Server.Exchange
  ( Request(..)
  , Response(..)
  , ResponseError(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:))

import Colorless.Types (RuntimeError, Limits)

data Request = Request
  { meta :: Value
  , query :: Value
  } deriving (Show, Eq)

instance FromJSON Request where
  parseJSON (Object o) = Request
    <$> o .: "meta"
    <*> o .: "query"
  parseJSON _ = mzero

instance ToJSON Request where
  toJSON Request{meta,query} = object [ "meta" .= meta, "query" .= query ]

data ResponseError
  = ResponseError'Service Value
  | ResponseError'Runtime RuntimeError
  deriving (Eq, Show)

instance ToJSON ResponseError where
  toJSON = \case
    ResponseError'Service m -> object [ "tag" .= String "Service", "service" .= m ]
    ResponseError'Runtime m -> object [ "tag" .= String "Runtime", "runtime" .= m ]

data Response
  = Response'Error ResponseError
  | Response'Success Value Limits
  deriving (Show, Eq)

instance ToJSON Response where
  toJSON (Response'Error m) = object [ "tag" .= String "Error", "error" .= m ]
  toJSON (Response'Success m limits) = object [ "tag" .= String "Success", "success" .= m, "limits" .= limits ]
