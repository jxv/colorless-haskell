{-# LANGUAGE DeriveGeneric #-}
module Colorless.Types
  ( Version(..)
  , Major(..)
  , Minor(..)
  , Request(..)
  , Response(..)
  , ResponseError(..)
  , RuntimeError(..)
  , RuntimeThrower(..)
  , Options(..)
  --
  , Symbol(..)
  , Type(..)
  , TypeName(..)
  , EnumeralName(..)
  , MemberName(..)
  , Const(..)
  ) where

import qualified Data.HashMap.Lazy as HML
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.String (IsString)
import Data.Scientific
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
  , query :: Value
  } deriving (Show, Eq)

instance FromJSON Request where
  parseJSON (Object o) = Request
    <$> o .: "meta"
    <*> o .: "query"
  parseJSON _ = mzero

data RuntimeError
  = RuntimeError'UnparsableFormat
  | RuntimeError'UnrecognizedCall
  | RuntimeError'VariableLimit
  | RuntimeError'UnknownVariable Text
  | RuntimeError'IncompatibleType
  | RuntimeError'TooFewArguments
  | RuntimeError'TooManyArguments
  | RuntimeError'NoApiVersion
  | RuntimeError'NoColorlessVersion
  | RuntimeError'ApiMajorVersionTooLow
  | RuntimeError'ApiMajorVersionTooHigh
  | RuntimeError'ApiMinorVersionTooHigh
  | RuntimeError'ColorlessMajorVersionTooLow
  | RuntimeError'ColorlessMajorVersionTooHigh
  | RuntimeError'ColorlessMinorVersionTooHigh
  | RuntimeError'UnparsableMeta
  | RuntimeError'UnparsableQuery
  | RuntimeError'NoImplementation
  | RuntimeError'NotMember
  deriving (Show, Eq)

instance ToJSON RuntimeError where
  toJSON = \case
    RuntimeError'UnparsableFormat -> e "UnparsableFormat"
    RuntimeError'UnrecognizedCall -> object [ "tag" .= String "UnrecognizedCall" ]
    RuntimeError'VariableLimit -> e "VariableLimit"
    RuntimeError'UnknownVariable m -> object [ "tag" .= String "UnknownVariable", "name" .= m ]
    RuntimeError'IncompatibleType -> e "IncompatibleType"
    RuntimeError'TooFewArguments -> e "TooFewArguments"
    RuntimeError'TooManyArguments -> e "TooManyArguments"
    RuntimeError'NoApiVersion -> e "NoApiVersion"
    RuntimeError'NoColorlessVersion -> e "NoColorlessVersion"
    RuntimeError'ApiMajorVersionTooHigh -> e "ApiMajorVersionTooHigh"
    RuntimeError'ApiMajorVersionTooLow -> e "ApiMajorVersionTooLow"
    RuntimeError'ApiMinorVersionTooHigh -> e "ApiMinorVersionTooHigh"
    RuntimeError'ColorlessMajorVersionTooHigh -> e "ColorlessMajorVersionTooHigh"
    RuntimeError'ColorlessMajorVersionTooLow -> e "ColorlessMajorVersionTooLow"
    RuntimeError'ColorlessMinorVersionTooHigh -> e "ColorlessMinorVersionTooHigh"
    RuntimeError'UnparsableMeta -> e "UnparsableMeta"
    RuntimeError'UnparsableQuery -> e "UnparsableQuery"
    RuntimeError'NoImplementation -> e "NoImplementation"
    RuntimeError'NotMember -> e "NotMember"
    where
      e s = object [ "tag" .= String s ]

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
  | Response'Success Value
  deriving (Show, Eq)

instance ToJSON Response where
  toJSON (Response'Error m) = object [ "tag" .= String "Error", "error" .= m ]
  toJSON (Response'Success m) = object [ "tag" .= String "Success", "success" .= m ]

class Monad m => RuntimeThrower m where
  runtimeThrow :: RuntimeError -> m a

instance RuntimeThrower IO where
  runtimeThrow err = error $ "Runtime error - " ++ show err

data Options = Options
  { variableLimit :: Maybe Int
  } deriving (Show, Eq)

--

newtype Symbol = Symbol Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

data Type = Type
  { n :: TypeName
  , p :: Maybe Type
  } deriving (Show, Eq)

instance FromJSON Type where
  parseJSON = \case
    String s -> pure $ Type (TypeName s) Nothing
    Object o -> do
      n <- o .: "n"
      case HML.lookup "p" o of
        Nothing -> pure $ Type n Nothing
        Just p -> Type n <$> fmap Just (parseJSON p)
    _ -> mzero

newtype TypeName = TypeName Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

newtype EnumeralName = EnumeralName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, IsString)

newtype MemberName = MemberName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey, IsString)

data Const
  = Const'Null
  | Const'Bool Bool
  | Const'String Text
  | Const'Number Scientific
  deriving (Show, Eq)

instance ToJSON Const where
  toJSON = \case
    Const'Null -> Null
    Const'Bool b -> Bool b
    Const'String s -> String s
    Const'Number n -> Number n

instance FromJSON Const where
  parseJSON = \case
    Null -> pure $ Const'Null
    Bool b -> pure $ Const'Bool b
    String s -> pure $ Const'String s
    Number n -> pure $ Const'Number n
    _ -> mzero
