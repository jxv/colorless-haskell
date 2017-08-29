{-# LANGUAGE DeriveGeneric #-}
module Colorless.Types
  ( Version(..)
  , Major(..)
  , Minor(..)
  , Request(..)
  , Response(..)
  , ResponseError(..)
  , RuntimeError(..)
  , Transport(..)
  , ToTransport(..)
  , FromTransport(..)
  , RuntimeThrower(..)
  , Options(..)
  , decodeTransport
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Aeson.Types (Parser)
import GHC.Generics

newtype Major = Major Int
  deriving (Show, Eq, Generic, Num, Ord, Real, Integral, Enum)

instance FromJSON Major
instance ToJSON Major

newtype Minor = Minor Int
  deriving (Show, Eq, Generic, Num, Ord, Real, Integral, Enum)

instance FromJSON Minor
instance ToJSON Minor

data Version = Version
  { major :: Major
  , minor :: Minor
  } deriving (Show, Eq, Generic)

instance ToJSON Version
instance FromJSON Version

data Request = Request
  { meta :: Value
  , calls :: [Value]
  } deriving (Show, Eq)

instance FromJSON Request where
  parseJSON (Object o) = Request
    <$> o .: "meta"
    <*> o .: "calls"
  parseJSON _ = mzero

data RuntimeError
  = RuntimeError'UnparsableFormat
  | RuntimeError'UnrecognizedCall Value
  | RuntimeError'VariableLimit
  | RuntimeError'UnknownVariable Text
  | RuntimeError'IncompatibleType
  | RuntimeError'TooFewArguments
  | RuntimeError'TooManyArguments
  | RuntimeError'NoApiVersion
  | RuntimeError'NoColorlessVersion
  | RuntimeError'ApiVersionTooLow
  | RuntimeError'ApiVersionTooHigh
  | RuntimeError'ColorlessVersionTooLow
  | RuntimeError'ColorlessVersionTooHigh
  | RuntimeError'UnparsableMeta
  | RuntimeError'UnparsableCalls
  | RuntimeError'NoImplementation
  deriving (Show, Eq)

instance ToJSON RuntimeError where
  toJSON = \case
    RuntimeError'UnparsableFormat -> object [ "e" .= String "UnparsableFormat" ]
    RuntimeError'UnrecognizedCall m -> object [ "e" .= String "UnrecognizedCall", "m" .= object [ "call" .= m ] ]
    RuntimeError'VariableLimit -> object [ "e" .= String "VariableLimit" ]
    RuntimeError'UnknownVariable m -> object [ "e" .= String "UnknownVariable", "m" .= object [ "name" .= m ] ]
    RuntimeError'IncompatibleType -> object [ "e" .= String "IncompatibleType" ]
    RuntimeError'TooFewArguments -> object [ "e" .= String "TooFewArguments" ]
    RuntimeError'TooManyArguments -> object [ "e" .= String "TooManyArguments" ]
    RuntimeError'NoApiVersion -> object [ "e" .= String "NoApiVersion" ]
    RuntimeError'NoColorlessVersion -> object [ "e" .= String "NoColorlessVersion" ]
    RuntimeError'ApiVersionTooHigh -> object [ "e" .= String "ApiVersionTooHigh" ]
    RuntimeError'ApiVersionTooLow -> object [ "e" .= String "ApiVersionTooLow" ]
    RuntimeError'ColorlessVersionTooHigh -> object [ "e" .= String "ColorlessVersionTooHigh" ]
    RuntimeError'ColorlessVersionTooLow -> object [ "e" .= String "ColorlessVersionTooLow" ]
    RuntimeError'UnparsableMeta -> object [ "e" .= String "UnparsableMeta" ]
    RuntimeError'UnparsableCalls -> object [ "e" .= String "UnparsableCalls" ]
    RuntimeError'NoImplementation -> object [ "e" .= String "NoImplementation" ]

data ResponseError
  = ResponseError'Service Value
  | ResponseError'Runtime RuntimeError
  deriving (Eq, Show)

instance ToJSON ResponseError where
  toJSON = \case
    ResponseError'Service m -> object [ "e" .= String "Service", "m" .= (object [ "service" .= m ]) ]
    ResponseError'Runtime m -> object [ "e" .= String "Runtime", "m" .= (object [ "runtime" .= m ]) ]

data Response
  = Response'Error ResponseError
  | Response'Success Value
  deriving (Show, Eq)

instance ToJSON Response where
  toJSON (Response'Error m) = object [ "e" .= String "Error", "m" .= (object [ "error" .= m ]) ]
  toJSON (Response'Success m) = object [ "e" .= String "Success", "m" .= (object [ "success" .= m ]) ]

newtype Transport a = Transport { unTransport :: a }
  deriving (Show, Eq)

class ToTransport a where
  toTransport :: FromJSON a => Value -> Parser (Transport a)
  toTransport v = Transport <$> parseJSON v

instance ToTransport Bool
instance ToTransport Int
instance ToTransport Integer
instance ToTransport Float
instance ToTransport Double
instance ToTransport Text
instance ToTransport Char
instance ToTransport a => ToTransport (Maybe a)
instance ToTransport a => ToTransport [a]

class FromTransport a where
  fromTransport :: ToJSON a => Transport a -> Value
  fromTransport (Transport a) = toJSON a

decodeTransport :: (ToJSON a, FromTransport a) => a -> Value
decodeTransport = fromTransport . Transport

instance FromTransport Bool
instance FromTransport Int
instance FromTransport Integer
instance FromTransport Float
instance FromTransport Double
instance FromTransport Text
instance FromTransport Char
instance FromTransport a => FromTransport (Maybe a)
instance FromTransport a => FromTransport [a]

instance FromTransport () where
  fromTransport (Transport ()) = object [ "e" .= String "Unit" ]

instance (FromTransport a, FromTransport b, ToJSON a, ToJSON b) => FromTransport (Either a b) where
  fromTransport (Transport (Left a)) = object
    [ "e" .= String "Left"
    , "m" .= object [ "left" .= decodeTransport a ]
    ]
  fromTransport (Transport (Right b)) = object
    [ "e" .= String "Right"
    , "m" .= object [ "right" .= decodeTransport b ]
    ]

class Monad m => RuntimeThrower m where
  runtimeThrow :: RuntimeError -> m a

instance RuntimeThrower IO where
  runtimeThrow err = error $ "Runtime error - " ++ show err

data Options = Options
  { variableLimit :: Maybe Int
  } deriving (Show, Eq)
