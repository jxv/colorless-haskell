module Fluid.Prim
  ( Prim(..)
  ) where

import Data.Aeson
import Data.Text (Text)

data Prim
  = Prim'Bool Bool
  | Prim'Int Int
  | Prim'Float Double
  | Prim'String Text
  deriving (Show, Eq)

instance ToJSON Prim where
  toJSON = \case
    Prim'Bool b -> toJSON b
    Prim'Int i -> toJSON i
    Prim'Float f -> toJSON f
    Prim'String s -> toJSON s
