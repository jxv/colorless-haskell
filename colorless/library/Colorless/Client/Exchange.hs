module Colorless.Client.Exchange
  ( Request(..)
  , Response(..)
  , ResponseError(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:))
import Data.Text (Text)

import Colorless.Types (RuntimeError, HasType(..), Version(..))
import Colorless.Ast (ToAst(..))
import Colorless.Client.Expr (Expr, exprJSON)

data Request meta a = Request
  { colorless :: Version
  , version :: Version
  , meta :: meta
  , query :: Expr a
  }

instance (ToAst meta, HasType meta, ToAst a, HasType a) => ToJSON (Request meta a) where
  toJSON Request{colorless, version, meta, query} = object
    [ "colorless" .= colorless
    , "version" .= version
    , "meta" .= toAst meta
    , "query" .= exprJSON query
    ]

data ResponseError err
  = ResponseError'Service err
  | ResponseError'Runtime RuntimeError
  deriving (Eq, Show)

instance (FromJSON err, HasType err) => FromJSON (ResponseError err) where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag :: Text of
      "Service" -> ResponseError'Service <$> o .: "service"
      "Runtime" -> ResponseError'Runtime <$> o .: "runtime"
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON err => ToJSON (ResponseError err) where
  toJSON = \case
    ResponseError'Service m -> object [ "tag" .= String "Service", "service" .= m ]
    ResponseError'Runtime m -> object [ "tag" .= String "Runtime", "runtime" .= m ]

data Response err a
  = Response'Error (ResponseError err)
  | Response'Success a
  deriving (Show, Eq)

instance (FromJSON err, HasType err, FromJSON a, HasType a) => FromJSON (Response err a) where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag :: Text of
      "Success" -> Response'Success <$> o .: "success"
      "Error" -> Response'Error <$> o .: "error"
      _ -> mzero
  parseJSON _ = mzero

instance (ToJSON err, ToJSON a) => ToJSON (Response err a) where
  toJSON (Response'Error m) = object [ "tag" .= String "Error", "error" .= m ]
  toJSON (Response'Success m) = object [ "tag" .= String "Success", "success" .= m ]
