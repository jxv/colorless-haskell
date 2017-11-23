{-# LANGUAGE DeriveGeneric #-}
module Fluid.Ast
  ( Ast(..)
  , ToAst(..)
  , Ref(..)
  , If(..)
  , Iflet(..)
  , Get(..)
  , Set(..)
  , Define(..)
  , Match(..)
  , MatchCase(..)
  , Lambda(..)
  , List(..)
  , Tuple(..)
  , Do(..)
  , FnCall(..)
  , Enumeral(..)
  , Struct(..)
  , Wrap(..)
  , StructCall(..)
  , WrapCall(..)
  , EnumerationCall(..)
  , HollowCall(..)
  , Const(..)
  ) where

import qualified Data.HashMap.Lazy as HML
import qualified Data.Vector as V
import qualified Data.Map as Map
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Aeson
import Data.Map (Map)
import GHC.Generics (Generic)

import Fluid.Types

data Ast
  = Ast'Ref Ref
  | Ast'If If
  | Ast'Iflet Iflet
  | Ast'Get Get
  | Ast'Set Set
  | Ast'Define Define
  | Ast'Match Match
  | Ast'Lambda Lambda
  | Ast'List List
  | Ast'Tuple Tuple
  | Ast'Do Do
  | Ast'FnCall FnCall
  | Ast'WrapCall WrapCall
  | Ast'StructCall StructCall
  | Ast'EnumerationCall EnumerationCall
  | Ast'HollowCall HollowCall
  | Ast'Enumeral Enumeral
  | Ast'Struct Struct
  | Ast'Wrap Wrap
  | Ast'Const Const
  deriving (Show, Eq)

class ToAst a where
  toAst :: a -> Ast

instance ToAst () where
  toAst () = Ast'Const Const'Null

instance ToAst Bool where
  toAst b = Ast'Const (Const'Bool b)

instance ToAst Text where
  toAst s = Ast'Const (Const'String s)

instance ToAst Int where
  toAst = num

instance ToAst Double where
  toAst = num

num :: (Num a, ToJSON a) => a -> Ast
num n = case toJSON n of
  Number a -> Ast'Const (Const'Number a)
  _ -> error "should never reach here"

instance ToAst a => ToAst [a] where
  toAst xs = Ast'List $ List $ map toAst xs

instance ToAst a => ToAst (Maybe a) where
  toAst Nothing = Ast'Const Const'Null
  toAst (Just x) = toAst x

instance (ToAst a, ToAst b) => ToAst (Either a b) where
  toAst (Left a) = Ast'Enumeral $ Enumeral "Left" $ Just $ Map.fromList [("left", toAst a)]
  toAst (Right a) = Ast'Enumeral $ Enumeral "Right" $ Just $ Map.fromList [("right", toAst a)]

--

instance (ToAst t1, ToAst t2) => ToAst (t1, t2) where
  toAst (t1, t2) = Ast'Tuple $ Tuple [toAst t1, toAst t2]

instance (ToAst t1, ToAst t2, ToAst t3) => ToAst (t1, t2, t3) where
  toAst (t1, t2, t3) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4) => ToAst (t1, t2, t3, t4) where
  toAst (t1, t2, t3, t4) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5) => ToAst (t1, t2, t3, t4, t5) where
  toAst (t1, t2, t3, t4, t5) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6) => ToAst (t1, t2, t3, t4, t5, t6) where
  toAst (t1, t2, t3, t4, t5, t6) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7) => ToAst (t1, t2, t3, t4, t5, t6, t7) where
  toAst (t1, t2, t3, t4, t5, t6, t7) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30, ToAst t31) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30, ToAst t31, ToAst t32) => ToAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
  toAst (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) = Ast'Tuple $ Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31, toAst t32]

{-
var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var toAstTuple = n => {
  if (n < 2) {
    return '';
  }

  var l = ['instance (ToAst t1'];
  for (var i = 1; i < n; i++) {
    l = l.concat([', ToAst t', i + 1]);
  }
  l = l.concat([') => ToAst (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') where\n']);

  l = l.concat(['  toAst (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') = Ast\'Tuple $ Tuple [toAst t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', toAst t', i + 1]);
  }
  l = l.concat([']\n\n']);

  return l.join('');
};
-}

--

instance FromJSON Ast where
  parseJSON v
    =   (Ast'Ref <$> parseJSON v)
    <|> (Ast'If <$> parseJSON v)
    <|> (Ast'Iflet <$> parseJSON v)
    <|> (Ast'Get <$> parseJSON v)
    <|> (Ast'Set <$> parseJSON v)
    <|> (Ast'Define <$> parseJSON v)
    <|> (Ast'Match <$> parseJSON v)
    <|> (Ast'Lambda <$> parseJSON v)
    <|> (Ast'List <$> parseJSON v)
    <|> (Ast'Tuple <$> parseJSON v)
    <|> (Ast'Do <$> parseJSON v)
    <|> (Ast'FnCall <$> parseJSON v)
    <|> (Ast'EnumerationCall <$> parseJSON v)
    <|> (Ast'WrapCall <$> parseJSON v)
    <|> (Ast'StructCall <$> parseJSON v)
    <|> (Ast'HollowCall <$> parseJSON v)
    <|> (Ast'Enumeral <$> parseJSON v)
    <|> (Ast'Struct <$> parseJSON v)
    <|> (Ast'Const <$> parseJSON v)

instance ToJSON Ast where
  toJSON = \case
    Ast'Ref a -> toJSON a
    Ast'If a -> toJSON a
    Ast'Iflet a -> toJSON a
    Ast'Get a -> toJSON a
    Ast'Set a -> toJSON a
    Ast'Define a -> toJSON a
    Ast'Match a -> toJSON a
    Ast'Lambda a -> toJSON a
    Ast'List a -> toJSON a
    Ast'Tuple a -> toJSON a
    Ast'Do a -> toJSON a
    Ast'FnCall a -> toJSON a
    Ast'EnumerationCall a -> toJSON a
    Ast'WrapCall a -> toJSON a
    Ast'StructCall a -> toJSON a
    Ast'HollowCall a -> toJSON a
    Ast'Enumeral a -> toJSON a
    Ast'Struct a -> toJSON a
    Ast'Wrap a -> toJSON a
    Ast'Const a -> toJSON a

data Ref = Ref
  { symbol :: Symbol
  } deriving (Show, Eq)

instance FromJSON Ref where
  parseJSON (Object o) = Ref <$> o .: "@"
  parseJSON _ = mzero

instance ToJSON Ref where
  toJSON Ref{symbol} = object [ "@" .= symbol ]

data If = If
  { cond :: Ast
  , true :: Ast
  , false :: Ast
  } deriving (Show, Eq)

instance FromJSON If where
  parseJSON (Array arr) = case V.toList arr of
    ["if", cond, true, false] -> If <$> parseJSON cond <*> parseJSON true <*> parseJSON false
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON If where
  toJSON If{cond,true,false} = toJSON ["if", toJSON cond, toJSON true, toJSON false]

data Iflet = Iflet
  { symbol :: Symbol
  , option :: Ast
  , some :: Ast
  , none :: Ast
  } deriving (Show, Eq)

instance FromJSON Iflet where
  parseJSON (Array arr) = case V.toList arr of
    ["iflet", symbol, option, some, none] -> Iflet <$> parseJSON symbol <*> parseJSON option <*> parseJSON some <*> parseJSON none
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Iflet where
  toJSON Iflet{symbol,option,some,none} = toJSON ["iflet", toJSON symbol, toJSON option, toJSON some, toJSON none]

data Get = Get
  { path :: [Text]
  , val :: Ast
  } deriving (Show, Eq)

instance FromJSON Get where
  parseJSON (Array arr) = case V.toList arr of
    ["get", path, val] -> Get <$> parseJSON path <*> parseJSON val
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Get where
  toJSON Get{path,val} = toJSON ["get", toJSON path, toJSON val]

data Set = Set
  { path :: [Text]
  , src :: Ast
  , dest :: Ast
  } deriving (Show, Eq)

instance FromJSON Set where
  parseJSON (Array arr) = case V.toList arr of
    ["set", path, src, dest] -> Set <$> parseJSON path <*> parseJSON src <*> parseJSON dest
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Set where
  toJSON Set{path,src,dest} = toJSON ["set", toJSON path, toJSON src, toJSON dest]

data Define = Define
  { var :: Symbol
  , expr :: Ast
  } deriving (Show, Eq)

instance FromJSON Define where
  parseJSON (Array arr) = case V.toList arr of
    ["def", var, expr] -> Define <$> parseJSON var <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Define where
  toJSON Define{var,expr} = toJSON ["def", toJSON var, toJSON expr]

data MatchCase
  = MatchCase'Tag EnumeralName Ast
  | MatchCase'Members EnumeralName Symbol Ast
  deriving (Show, Eq)

instance ToJSON MatchCase where
  toJSON (MatchCase'Tag name ast) = toJSON [toJSON name, toJSON ast]
  toJSON (MatchCase'Members name sym ast) = toJSON [toJSON name, toJSON sym, toJSON ast]

instance FromJSON MatchCase where
  parseJSON (Array arr) = case V.toList arr of
    [name, ast] -> MatchCase'Tag <$> parseJSON name <*> parseJSON ast
    [name, sym, ast] -> MatchCase'Members <$> parseJSON name <*> parseJSON sym <*> parseJSON ast
    _ -> mzero
  parseJSON _ = mzero

data Match = Match
  { enumeral :: Ast
  , cases :: [MatchCase]
  } deriving (Show, Eq)

instance FromJSON Match where
  parseJSON (Array arr) = case V.toList arr of
    ("match":enumeral:cases) -> Match <$> parseJSON enumeral <*> mapM parseJSON cases
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Match where
  toJSON Match{enumeral, cases} = toJSON $ ["match", toJSON enumeral] ++ map toJSON cases

data Lambda = Lambda
  { args :: [(Symbol, Type)]
  , expr :: Ast
  } deriving (Show, Eq)

instance FromJSON Lambda where
  parseJSON (Array arr) = case V.toList arr of
    ["fn", Array params, expr] -> Lambda <$> mapM parseParam (V.toList params) <*> parseJSON expr
    _ -> mzero
    where
      parseParam = \case
        Object o -> case HML.toList o of
          [(key,val)] -> (Symbol key,) <$> parseJSON val
          _ -> mzero
        _ -> mzero
  parseJSON _ = mzero

instance ToJSON Lambda where
  toJSON Lambda{args,expr} = toJSON ["fn", toJSON $ map (Map.fromList . (:[])) args, toJSON expr]

data List = List
  { list :: [Ast]
  } deriving (Show, Eq)

instance FromJSON List where
  parseJSON (Array arr) = case V.toList arr of
    ("list":(Array xs):[]) -> List <$> mapM parseJSON (V.toList xs)
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON List where
  toJSON List{list} = toJSON $ "list" : [toJSON list]

data Tuple = Tuple
  { tuple :: [Ast]
  } deriving (Show, Eq)

instance FromJSON Tuple where
  parseJSON (Array arr) = case V.toList arr of
    ("tuple":xs) -> Tuple <$> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Tuple where
  toJSON Tuple{tuple} = toJSON $ "tuple" : map toJSON tuple

data Do = Do
  { vals :: [Ast]
  } deriving (Show, Eq)

instance FromJSON Do where
  parseJSON (Array arr) = case V.toList arr of
    "do":xs -> Do <$> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Do where
  toJSON Do{vals} = toJSON $ "do" : map toJSON vals

data FnCall = FnCall
  { fn :: Ast
  , args :: [Ast]
  } deriving (Show, Eq)

instance FromJSON FnCall where
  parseJSON (Array arr) = case V.toList arr of
    ((String x):xs) -> FnCall <$> pure (Ast'Ref $ Ref $ Symbol x) <*> mapM parseJSON xs
    (x:xs) -> FnCall <$> parseJSON x <*> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON FnCall where
  toJSON (FnCall (Ast'Ref (Ref sym)) args) = toJSON $ toJSON sym : map toJSON args
  toJSON (FnCall fn args) = toJSON $ fn : args

data EnumerationCall = EnumerationCall
  { n :: TypeName
  , e :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON EnumerationCall
instance ToJSON EnumerationCall

data WrapCall = WrapCall
  { n :: TypeName
  , w :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON WrapCall
instance ToJSON WrapCall

data StructCall = StructCall
  { n :: TypeName
  , m :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON StructCall
instance ToJSON StructCall

data HollowCall = HollowCall
  { n :: TypeName
  } deriving (Show, Eq, Generic)

instance FromJSON HollowCall
instance ToJSON HollowCall

data Enumeral = Enumeral
  { tag :: EnumeralName
  , m :: Maybe (Map MemberName Ast)
  } deriving (Show, Eq, Generic)

instance FromJSON Enumeral where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    let tagless = HML.delete "tag" o
    if HML.size o == 1
      then pure $ Enumeral tag Nothing
      else Enumeral tag <$> (Just <$> parseJSON (Object tagless))
  parseJSON _ = mzero

instance ToJSON Enumeral where
  toJSON Enumeral{tag, m} = case m of
    Nothing -> object [ "tag" .= tag ]
    Just m' -> toJSON $ Map.fromList $ ("tag", toJSON tag) : Map.toList (toJSON <$> m')

data Struct = Struct
  { m :: Map MemberName Ast
  } deriving (Show, Eq, Generic)

instance FromJSON Struct where
  parseJSON v = Struct <$> parseJSON v

instance ToJSON Struct where
  toJSON (Struct m) = toJSON m

data Wrap = Wrap
  { w :: Ast
  } deriving (Show, Eq, Generic)

-- Wrap does not have a FromJSON instance
-- 'Wrapped' values will be coerced as needed

instance ToJSON Wrap where
  toJSON (Wrap w) = toJSON w
