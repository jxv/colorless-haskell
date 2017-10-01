module Colorless.Server.Types
  ( Type(..)
  , TypeName(..)
  , MemberName(..)
  , EnumeralName(..)
  , EnumeralType(..)
  , EnumerationType(..)
  , StructType(..)
  , HollowType(..)
  , Prim(..)
  ) where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import Data.Map (Map)
import Data.Int
import Data.Word

import Colorless.Types

data EnumeralType = EnumeralType
  { m :: Maybe (Map MemberName Type)
  } deriving (Show, Eq)

data EnumerationType = EnumerationType
  { e :: Map EnumeralName EnumeralType
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
  | Prim'U8 Word8
  | Prim'U16 Word16
  | Prim'U32 Word32
  | Prim'U64 Word64
  | Prim'String Text
  deriving (Show, Eq)

instance ToJSON Prim where
  toJSON = \case
    Prim'Bool b -> toJSON b
    Prim'I8 i -> toJSON i
    Prim'I16 i -> toJSON i
    Prim'I32 i -> toJSON i
    Prim'I64 i -> toJSON i
    Prim'U8 u -> toJSON u
    Prim'U16 u -> toJSON u
    Prim'U32 u -> toJSON u
    Prim'U64 u -> toJSON u
    Prim'String s -> toJSON s
