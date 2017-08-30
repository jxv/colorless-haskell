module Colorless.Runtime.Types
  ( Symbol(..)
  , Type(..)
  , TypeName(..)
  , MemberName(..)
  , EnumeratorName(..)
  , EnumeratorType(..)
  , EnumerationType(..)
  , StructType(..)
  , ZeroArityType(..)
  , TypeEntry(..)
  , TypeDict
  ) where

import qualified Data.HashMap.Lazy as HML
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), FromJSONKey, Value(..), (.:))
import Data.Text (Text)
import Data.Map (Map)
import Data.String (IsString)

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

newtype EnumeratorName = EnumeratorName Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

newtype MemberName = MemberName Text
  deriving (Show, Eq, Ord, FromJSON, FromJSONKey, IsString)

data EnumeratorType = EnumeratorType
  { m :: Maybe (Map MemberName Type)
  } deriving (Show, Eq)

data EnumerationType = EnumerationType
  { e :: Map EnumeratorName EnumeratorType
  , o :: Type
  } deriving (Show, Eq)

data StructType = StructType
  { m :: Map MemberName Type
  , o :: Type
  } deriving (Show, Eq)

data ZeroArityType = ZeroArityType
  { o :: Type
  } deriving (Show, Eq)

data TypeEntry
  = TypeEntry'EnumerationType EnumerationType
  | TypeEntry'StructType StructType
  | TypeEntry'ZeroArityType ZeroArityType
  deriving (Show, Eq)

type TypeDict = Map TypeName TypeEntry
