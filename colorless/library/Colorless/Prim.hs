module Colorless.Prim
  ( Prim(..)
  ) where

import Data.Aeson
import Data.Word
import Data.Int
import Data.Text (Text)

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
  | Prim'F32 Float
  | Prim'F64 Double
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
    Prim'F32 f -> toJSON f
    Prim'F64 f -> toJSON f
    Prim'String s -> toJSON s
