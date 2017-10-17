{-# LANGUAGE DeriveGeneric #-}
module Fluid.Val
  ( Val(..)
  , ApiVal(..)
  , Wrap(..)
  , Struct(..)
  , Enumeral(..)
  --
  , FromVal(..)
  , ToVal(..)
  , getMember
  , fromValFromJson
  , combineObjects
  ) where

import qualified Data.HashMap.Lazy as HML
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map (Map)
import Data.Text (Text)
import Data.Int
import Data.Word
import Data.Scientific
import GHC.Generics (Generic)

import Fluid.Types
import Fluid.Prim

data Val
  = Val'Const Const
  | Val'Prim Prim
  | Val'ApiVal ApiVal
  | Val'List [Val]
  deriving (Show, Eq)

instance ToJSON Val where
  toJSON = \case
    Val'Const c -> toJSON c
    Val'ApiVal v -> toJSON v
    Val'List l -> toJSON l
    Val'Prim p -> toJSON p

instance FromJSON Val where
  parseJSON = \case
    Null -> return $ Val'Const Const'Null
    Number n -> return $ Val'Const $ Const'Number n
    String s -> return $ Val'Const $ Const'String s
    Bool b -> return $ Val'Const $ Const'Bool b
    Array arr -> Val'List <$> (mapM parseJSON $ V.toList arr)
    v@Object{} -> Val'ApiVal <$> parseJSON v

data ApiVal
  = ApiVal'Struct Struct
  | ApiVal'Enumeral Enumeral
  deriving (Show, Eq)

instance ToJSON ApiVal where
  toJSON = \case
    ApiVal'Struct s -> toJSON s
    ApiVal'Enumeral e -> toJSON e

instance FromJSON ApiVal where
  parseJSON v =
    (ApiVal'Enumeral <$> parseJSON v) <|>
    (ApiVal'Struct <$> parseJSON v)

data Wrap = Wrap
  { w :: Const
  } deriving (Show, Eq)

data Struct = Struct
  { m :: Map MemberName Val
  } deriving (Show, Eq, Generic)

instance ToJSON Struct where
  toJSON Struct{m} = toJSON m

instance FromJSON Struct where
  parseJSON v = Struct <$> parseJSON v

data Enumeral = Enumeral
  { tag :: EnumeralName
  , m :: Maybe (Map MemberName Val)
  } deriving (Show, Eq, Generic)

instance ToJSON Enumeral where
  toJSON Enumeral{tag,m} = object $ [ "tag" .= tag ] ++ case m of
    Nothing -> []
    Just m' -> concatMap (\(MemberName k,v) -> [ k .= v ]) (Map.toList m')

instance FromJSON Enumeral where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    let tagless = HML.delete "tag" o
    if HML.size o == 1
      then pure $ Enumeral tag Nothing
      else Enumeral tag <$> (Just <$> parseJSON (Object tagless))
  parseJSON _ = mzero

--

class ToVal a where
  toVal :: a -> Val

instance ToVal () where
  toVal () = Val'Const Const'Null

instance ToVal Bool where
  toVal b = Val'Const $ Const'Bool b

instance ToVal Text where
  toVal s = Val'Const $ Const'String s

intToVal :: Integral a => a -> Val
intToVal n = Val'Const $ Const'Number (fromInteger $ toInteger n)

instance ToVal Int where
  toVal = intToVal

instance ToVal Int8 where
  toVal i = Val'Prim $ Prim'I8 i

instance ToVal Int16 where
  toVal i = Val'Prim $ Prim'I16 i

instance ToVal Int32 where
  toVal i = Val'Prim $ Prim'I32 i

instance ToVal Int64 where
  toVal i = Val'Prim $ Prim'I64 i

instance ToVal Word where
  toVal = intToVal

instance ToVal Word8 where
  toVal u = Val'Prim $ Prim'U8 u

instance ToVal Word16 where
  toVal u = Val'Prim $ Prim'U16 u

instance ToVal Word32 where
  toVal u = Val'Prim $ Prim'U32 u

instance ToVal Word64 where
  toVal u = Val'Prim $ Prim'U64 u

instance ToVal Float where
    toVal f = Val'Prim $ Prim'F32 f

instance ToVal Double where
    toVal d = Val'Prim $ Prim'F64 d

instance ToVal a => ToVal (Maybe a) where
  toVal Nothing = Val'Const Const'Null
  toVal (Just v) = toVal v

instance (ToVal a, ToVal b) => ToVal (Either a b) where
  toVal m = Val'ApiVal $ ApiVal'Enumeral $ case m of
    Left l -> Enumeral "Left" (Just $ Map.singleton "left" (toVal l))
    Right r -> Enumeral "Right" (Just $ Map.singleton "right" (toVal r))

instance ToVal a => ToVal [a] where
  toVal list = Val'List $ map toVal list

instance (ToVal t1, ToVal t2) => ToVal (t1, t2) where
  toVal (t1, t2) = Val'List [toVal t1, toVal t2]

instance (ToVal t1, ToVal t2, ToVal t3) => ToVal (t1, t2, t3) where
  toVal (t1, t2, t3) = Val'List [toVal t1, toVal t2, toVal t3]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4) => ToVal (t1, t2, t3, t4) where
  toVal (t1, t2, t3, t4) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5) => ToVal (t1, t2, t3, t4, t5) where
  toVal (t1, t2, t3, t4, t5) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6) => ToVal (t1, t2, t3, t4, t5, t6) where
  toVal (t1, t2, t3, t4, t5, t6) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7) => ToVal (t1, t2, t3, t4, t5, t6, t7) where
  toVal (t1, t2, t3, t4, t5, t6, t7) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27, ToVal t28) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27, toVal t28]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27, ToVal t28, ToVal t29) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27, toVal t28, toVal t29]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27, ToVal t28, ToVal t29, ToVal t30) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27, toVal t28, toVal t29, toVal t30]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27, ToVal t28, ToVal t29, ToVal t30, ToVal t31) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27, toVal t28, toVal t29, toVal t30, toVal t31]

instance (ToVal t1, ToVal t2, ToVal t3, ToVal t4, ToVal t5, ToVal t6, ToVal t7, ToVal t8, ToVal t9, ToVal t10, ToVal t11, ToVal t12, ToVal t13, ToVal t14, ToVal t15, ToVal t16, ToVal t17, ToVal t18, ToVal t19, ToVal t20, ToVal t21, ToVal t22, ToVal t23, ToVal t24, ToVal t25, ToVal t26, ToVal t27, ToVal t28, ToVal t29, ToVal t30, ToVal t31, ToVal t32) => ToVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
  toVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) = Val'List [toVal t1, toVal t2, toVal t3, toVal t4, toVal t5, toVal t6, toVal t7, toVal t8, toVal t9, toVal t10, toVal t11, toVal t12, toVal t13, toVal t14, toVal t15, toVal t16, toVal t17, toVal t18, toVal t19, toVal t20, toVal t21, toVal t22, toVal t23, toVal t24, toVal t25, toVal t26, toVal t27, toVal t28, toVal t29, toVal t30, toVal t31, toVal t32]

{-
var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var toValTuple = n => {
  if (n < 2) {
    return '';
  }

  var l = ['instance (ToVal t1'];
  for (var i = 1; i < n; i++) {
    l = l.concat([', ToVal t', i + 1]);
  }
  l = l.concat([') => ToVal (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') where\n']);

  l = l.concat(['  toVal (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') = Val\'List [toVal t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', toVal t', i + 1]);
  }
  l = l.concat([']\n\n']);
  return l.join('');
};
-}
--

class FromVal a where
  fromVal :: Val -> Maybe a

instance FromVal () where
  fromVal (Val'Const Const'Null) = Just ()
  fromVal _ = Nothing

instance FromVal Bool where
  fromVal (Val'Const (Const'Bool b)) = Just b
  fromVal _ = Nothing

instance FromVal Text where
  fromVal (Val'Const (Const'String s)) = Just s
  fromVal _ = Nothing

intFromVal :: (Bounded i, Integral i) => Val -> Maybe i
intFromVal (Val'Const (Const'Number n)) = toBoundedInteger n
intFromVal _ = Nothing

instance FromVal Int where
  fromVal = intFromVal

instance FromVal Int8 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'I8 i) -> Just i
    _ -> Nothing

instance FromVal Int16 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'I16 i) -> Just i
    _ -> Nothing

instance FromVal Int32 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'I32 i) -> Just i
    _ -> Nothing

instance FromVal Int64 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'I64 i) -> Just i
    _ -> Nothing

instance FromVal Word where
  fromVal = intFromVal

instance FromVal Word8 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'U8 u) -> Just u
    _ -> Nothing

instance FromVal Word16 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'U16 u) -> Just u
    _ -> Nothing

instance FromVal Word32 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'U32 u) -> Just u
    _ -> Nothing

instance FromVal Word64 where
  fromVal = \case
    Val'Const (Const'Number n) -> toBoundedInteger n
    Val'Prim (Prim'U64 u) -> Just u
    _ -> Nothing

instance FromVal Float where
  fromVal (Val'Const (Const'Number n)) = Just $ toRealFloat n
  fromVal (Val'Prim (Prim'F32 f)) = Just f
  fromVal _ = Nothing

instance FromVal Double where
  fromVal (Val'Const (Const'Number n)) = Just $ toRealFloat n
  fromVal (Val'Prim (Prim'F64 f)) = Just f
  fromVal _ = Nothing

instance FromVal a => FromVal (Maybe a) where
  fromVal (Val'Const Const'Null) = Just Nothing
  fromVal v = Just <$> fromVal v

instance (FromVal a, FromVal b) => FromVal (Either a b) where
  fromVal (Val'ApiVal (ApiVal'Enumeral (Enumeral tag m))) = case (tag,m) of
    ("Left",Just m') -> Map.lookup "left" m' >>= \l -> Left <$> fromVal l
    ("Right",Just m') -> Map.lookup "right" m' >>= \r -> Right <$> fromVal r
    _ -> Nothing
  fromVal _ = Nothing

instance FromVal a => FromVal [a] where
  fromVal (Val'List list) = mapM fromVal list
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2) => FromVal (t1, t2) where
  fromVal (Val'List [t1, t2]) = (,) <$> fromVal t1 <*> fromVal t2
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3) => FromVal (t1, t2, t3) where
  fromVal (Val'List [t1, t2, t3]) = (,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4) => FromVal (t1, t2, t3, t4) where
  fromVal (Val'List [t1, t2, t3, t4]) = (,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5) => FromVal (t1, t2, t3, t4, t5) where
  fromVal (Val'List [t1, t2, t3, t4, t5]) = (,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6) => FromVal (t1, t2, t3, t4, t5, t6) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6]) = (,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7) => FromVal (t1, t2, t3, t4, t5, t6, t7) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7]) = (,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8]) = (,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9]) = (,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]) = (,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]) = (,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12]) = (,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13]) = (,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14]) = (,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15]) = (,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]) = (,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17]) = (,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18]) = (,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19]) = (,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20]) = (,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21]) = (,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22]) = (,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23]) = (,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24]) = (,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25]) = (,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26]) = (,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27, FromVal t28) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27 <*> fromVal t28
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27, FromVal t28, FromVal t29) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27 <*> fromVal t28 <*> fromVal t29
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27, FromVal t28, FromVal t29, FromVal t30) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27 <*> fromVal t28 <*> fromVal t29 <*> fromVal t30
  fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27, FromVal t28, FromVal t29, FromVal t30, FromVal t31) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27 <*> fromVal t28 <*> fromVal t29 <*> fromVal t30 <*> fromVal t31
    fromVal _ = Nothing

instance (FromVal t1, FromVal t2, FromVal t3, FromVal t4, FromVal t5, FromVal t6, FromVal t7, FromVal t8, FromVal t9, FromVal t10, FromVal t11, FromVal t12, FromVal t13, FromVal t14, FromVal t15, FromVal t16, FromVal t17, FromVal t18, FromVal t19, FromVal t20, FromVal t21, FromVal t22, FromVal t23, FromVal t24, FromVal t25, FromVal t26, FromVal t27, FromVal t28, FromVal t29, FromVal t30, FromVal t31, FromVal t32) => FromVal (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    fromVal (Val'List [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32]) = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromVal t1 <*> fromVal t2 <*> fromVal t3 <*> fromVal t4 <*> fromVal t5 <*> fromVal t6 <*> fromVal t7 <*> fromVal t8 <*> fromVal t9 <*> fromVal t10 <*> fromVal t11 <*> fromVal t12 <*> fromVal t13 <*> fromVal t14 <*> fromVal t15 <*> fromVal t16 <*> fromVal t17 <*> fromVal t18 <*> fromVal t19 <*> fromVal t20 <*> fromVal t21 <*> fromVal t22 <*> fromVal t23 <*> fromVal t24 <*> fromVal t25 <*> fromVal t26 <*> fromVal t27 <*> fromVal t28 <*> fromVal t29 <*> fromVal t30 <*> fromVal t31 <*> fromVal t32
    fromVal _ = Nothing

{-
var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var fromValTuple = n => {
  if (n < 2) {
    return '';
  }

  var l = ['instance (FromVal t1'];
  for (var i = 1; i < n; i++) {
    l = l.concat([', FromVal t', i + 1]);
  }
  l = l.concat([') => FromVal (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') where\n']);

  l = l.concat(['  fromVal (Val\'List [t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([']) = (']);
  for (var i = 1; i < n; i++) {
    l = l.concat([',']);
  }
  l = l.concat([') <$> fromVal t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([' <*> fromVal t', i + 1]);
  }
  l = l.concat([
    '\n',
    '  fromVal _ = Nothing\n\n']);
  return l.join('');
};
-}

getMember :: FromVal a => Map MemberName Val -> MemberName -> Maybe a
getMember m n = fromVal =<< Map.lookup n m

fromValFromJson :: (FromVal b) => Value -> Maybe b
fromValFromJson x = fromVal =<< parseMaybe parseJSON x

combineObjects :: Value -> Value -> Value
combineObjects (Object x) (Object y) = Object $ HML.union x y
combineObjects _ _ = error "not objects"
