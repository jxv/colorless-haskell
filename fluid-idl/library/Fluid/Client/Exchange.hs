module Fluid.Client.Exchange
  ( Request(..)
  , Response(..)
  , ResponseError(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:))
import Data.Text (Text)

import Fluid.Types (RuntimeError, HasType(..), Version(..), Limits)
import Fluid.Ast (ToAst(..))
import Fluid.Val (ToVal(..), FromVal(..))
import Fluid.Client.Expr (Expr, exprJSON)

data Request meta a = Request
  { fluid :: Version
  , version :: Version
  , meta :: meta
  , query :: Expr a
  }

instance (ToVal meta, HasType meta, ToAst a, HasType a) => ToJSON (Request meta a) where
  toJSON Request{fluid, version, meta, query} = object
    [ "fluid" .= fluid
    , "version" .= version
    , "meta" .= toVal meta
    , "query" .= exprJSON query
    ]

data ResponseError err
  = ResponseError'Service err
  | ResponseError'Runtime RuntimeError
  deriving (Eq, Show)

instance (FromVal err, HasType err) => FromJSON (ResponseError err) where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag :: Text of
      "Service" -> do
        m <- o .: "service"
        case fromVal m of
          Nothing -> mzero
          Just v -> return $ ResponseError'Service v
      "Runtime" -> ResponseError'Runtime <$> o .: "runtime"
      _ -> mzero
  parseJSON _ = mzero

instance ToVal err => ToJSON (ResponseError err) where
  toJSON = \case
    ResponseError'Service m -> object [ "tag" .= String "Service", "service" .= toVal m ]
    ResponseError'Runtime m -> object [ "tag" .= String "Runtime", "runtime" .= m ]

data Response err a
  = Response'Error (ResponseError err)
  | Response'Success a Limits
  deriving (Show, Eq)

instance (FromVal err, HasType err, FromVal a, HasType a) => FromJSON (Response err a) where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag :: Text of
      "Success" -> do
        m <- o .: "success"
        case fromVal m of
          Nothing -> mzero
          Just v -> Response'Success v <$> o .: "limits"
      "Error" -> Response'Error <$> o .: "error"
      _ -> mzero
  parseJSON _ = mzero

instance (ToVal err, HasType err, ToVal a, HasType a) => ToJSON (Response err a) where
  toJSON (Response'Error m) = object [ "tag" .= String "Error", "error" .= m ]
  toJSON (Response'Success m limits) = object [ "tag" .= String "Success", "success" .= toVal m, "limits" .= limits]
