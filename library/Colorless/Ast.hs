{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Colorless.Ast
  ( Type(..)
  , TypeName(..)
  , MemberName(..)
  , Symbol(..)
  --
  , Ast(..)
  , Ref(..)
  , If(..)
  , Set(..)
  , Get(..)
  , Define(..)
  , Lambda(..)
  , List(..)
  , Begin(..)
  , FnCall(..)
  , Enumerator(..)
  , Struct(..)
  , StructCall(..)
  , EnumeratorCall(..)
  , ZeroArityCall(..)
  , Const(..)
  ) where

import qualified Data.HashMap.Lazy as HML
import qualified Data.Vector as V
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.String (IsString)
import Data.Aeson
import Data.Scientific
import Data.Map (Map)
import GHC.Generics (Generic)

data Ast
  = Ast'Void
  | Ast'Ref Ref
  | Ast'If If
  | Ast'Set Set
  | Ast'Get Get
  | Ast'Define Define
  | Ast'Lambda Lambda
  | Ast'List List
  | Ast'Begin Begin
  | Ast'FnCall FnCall
  | Ast'Struct Struct
  | Ast'Enumerator Enumerator
  | Ast'StructCall StructCall
  | Ast'EnumeratorCall EnumeratorCall
  | Ast'ZeroArityCall ZeroArityCall
  | Ast'Const Const
  deriving (Show, Eq)

instance FromJSON Ast where
  parseJSON Null = pure Ast'Void
  parseJSON v
    =   (Ast'Ref <$> parseJSON v)
    <|> (Ast'If <$> parseJSON v)
    <|> (Ast'Set <$> parseJSON v)
    <|> (Ast'Get <$> parseJSON v)
    <|> (Ast'Define <$> parseJSON v)
    <|> (Ast'Lambda <$> parseJSON v)
    <|> (Ast'List <$> parseJSON v)
    <|> (Ast'Begin <$> parseJSON v)
    <|> (Ast'FnCall <$> parseJSON v)
    <|> (Ast'EnumeratorCall <$> parseJSON v)
    <|> (Ast'StructCall <$> parseJSON v)
    <|> (Ast'ZeroArityCall <$> parseJSON v)
    <|> (Ast'Enumerator <$> parseJSON v)
    <|> (Ast'Struct <$> parseJSON v)
    <|> (Ast'Const <$> parseJSON v)

newtype Symbol = Symbol Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

data Ref = Ref
  { symbol :: Symbol
  } deriving (Show, Eq)

instance FromJSON Ref where
  parseJSON (Object o) = Ref <$> o .: "@"
  parseJSON _ = mzero

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

data Set = Set
  { var :: Symbol
  , expr :: Ast
  } deriving (Show, Eq)

instance FromJSON Set where
  parseJSON (Array arr) = case V.toList arr of
    ["set", var, expr] -> Set <$> parseJSON var <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

data Get = Get
  { path :: [Text]
  , val :: Ast
  } deriving (Show, Eq)

instance FromJSON Get where
  parseJSON (Array arr) = case V.toList arr of
    ["get", path, val] -> Get <$> parseJSON path <*> parseJSON val
    _ -> mzero
  parseJSON _ = mzero

data Define = Define
  { var :: Symbol
  , expr :: Ast
  } deriving (Show, Eq)

instance FromJSON Define where
  parseJSON (Array arr) = case V.toList arr of
    ["def", var, expr] -> Define <$> parseJSON var <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

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

data List = List
  { list :: [Ast]
  } deriving (Show, Eq)

instance FromJSON List where
  parseJSON (Object o) = List <$> o .: "List"
  parseJSON _ = mzero

data Begin = Begin
  { vals :: [Ast]
  } deriving (Show, Eq)

instance FromJSON Begin where
  parseJSON (Array arr) = case V.toList arr of
    "begin":xs -> Begin <$> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

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

newtype TypeName = TypeName Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

newtype EnumeratorName = EnumeratorName Text
  deriving (Show, Eq, Ord, FromJSON, IsString)

newtype MemberName = MemberName Text
  deriving (Show, Eq, Ord, FromJSON, FromJSONKey, IsString)

data EnumeratorCall = EnumeratorCall
  { n :: TypeName
  , e :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON EnumeratorCall

data StructCall = StructCall
  { n :: TypeName
  , m :: Map MemberName Ast
  } deriving (Show, Eq, Generic)

instance FromJSON StructCall

data ZeroArityCall = ZeroArityCall
  { n :: TypeName
  } deriving (Show, Eq, Generic)

instance FromJSON ZeroArityCall

data Enumerator = Enumerator
  { e :: EnumeratorName
  , m :: Maybe (Map MemberName Ast)
  } deriving (Show, Eq, Generic)

instance FromJSON Enumerator

data Struct = Struct
  { m :: Map MemberName Ast
  } deriving (Show, Eq, Generic)

instance FromJSON Struct

data Const
  = Const'Bool Bool
  | Const'String Text
  | Const'Number Scientific
  deriving (Show, Eq)

instance FromJSON Const where
  parseJSON = \case
    Bool b -> pure $ Const'Bool b
    String s -> pure $ Const'String s
    Number n -> pure $ Const'Number n
    _ -> mzero
