module Colorless.Runtime.Types
  ( Symbol(..)
  , Type(..)
  , TypeName(..)
  , MemberName(..)
  , EnumeratorName(..)
  , EnumeratorType(..)
  , EnumerationType(..)
  , StructType(..)
  , HollowType(..)
  , Prim(..)
  , Const(..)
  ) where

import qualified Data.HashMap.Lazy as HML
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), FromJSONKey, ToJSONKey, Value(..), (.:), ToJSON(..))
import Data.Text (Text)
import Data.Map (Map)
import Data.Int
import Data.Scientific
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
  deriving (Show, Eq, Ord, FromJSON, ToJSON, IsString)

newtype MemberName = MemberName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey, IsString)

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

data HollowType = HollowType
  { o :: Type
  } deriving (Show, Eq)

data Prim
  = Prim'Bool Bool
  | Prim'I8 Int8
  | Prim'I16 Int16
  | Prim'I32 Int32
  | Prim'I64 Int64
  | Prim'String Text
  deriving (Show, Eq)

instance ToJSON Prim where
  toJSON = \case
    Prim'Bool b -> toJSON b
    Prim'I8 i -> toJSON i
    Prim'I16 i -> toJSON i
    Prim'I32 i -> toJSON i
    Prim'I64 i -> toJSON i
    Prim'String s -> toJSON s

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
