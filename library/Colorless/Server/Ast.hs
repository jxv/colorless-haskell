{-# LANGUAGE DeriveGeneric #-}
module Colorless.Server.Ast
  ( Ast(..)
  , Ref(..)
  , If(..)
  , Get(..)
  , Define(..)
  , Lambda(..)
  , List(..)
  , Tuple(..)
  , Begin(..)
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
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Aeson
import Data.Map (Map)
import GHC.Generics (Generic)

import Colorless.Types
import Colorless.Server.Types

data Ast
  = Ast'Ref Ref
  | Ast'If If
  | Ast'Get Get
  | Ast'Define Define
  | Ast'Lambda Lambda
  | Ast'List List
  | Ast'Tuple Tuple
  | Ast'Begin Begin
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

instance FromJSON Ast where
  parseJSON v
    =   (Ast'Ref <$> parseJSON v)
    <|> (Ast'If <$> parseJSON v)
    <|> (Ast'Get <$> parseJSON v)
    <|> (Ast'Define <$> parseJSON v)
    <|> (Ast'Lambda <$> parseJSON v)
    <|> (Ast'List <$> parseJSON v)
    <|> (Ast'Tuple <$> parseJSON v)
    <|> (Ast'Begin <$> parseJSON v)
    <|> (Ast'FnCall <$> parseJSON v)
    <|> (Ast'EnumerationCall <$> parseJSON v)
    <|> (Ast'WrapCall <$> parseJSON v)
    <|> (Ast'StructCall <$> parseJSON v)
    <|> (Ast'HollowCall <$> parseJSON v)
    <|> (Ast'Enumeral <$> parseJSON v)
    <|> (Ast'Struct <$> parseJSON v)
    <|> (Ast'Wrap <$> parseJSON v)
    <|> (Ast'Const <$> parseJSON v)

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
  parseJSON (Array arr) = case V.toList arr of
    ("list":(Array xs):[]) -> List <$> mapM parseJSON (V.toList xs)
    _ -> mzero
  parseJSON _ = mzero

data Tuple = Tuple
  { tuple :: [Ast]
  } deriving (Show, Eq)

instance FromJSON Tuple where
  parseJSON (Array arr) = case V.toList arr of
    ("tuple":xs) -> Tuple <$> mapM parseJSON xs
    _ -> mzero
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

data EnumerationCall = EnumerationCall
  { n :: TypeName
  , e :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON EnumerationCall

data WrapCall = WrapCall
  { n :: TypeName
  , w :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON WrapCall

data StructCall = StructCall
  { n :: TypeName
  , m :: Map MemberName Ast
  } deriving (Show, Eq, Generic)

instance FromJSON StructCall

data HollowCall = HollowCall
  { n :: TypeName
  } deriving (Show, Eq, Generic)

instance FromJSON HollowCall

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

data Struct = Struct
  { m :: Map MemberName Ast
  } deriving (Show, Eq, Generic)

instance FromJSON Struct where
  parseJSON v = Struct <$> parseJSON v

data Wrap = Wrap
  { w :: Ast
  } deriving (Show, Eq, Generic)

instance FromJSON Wrap
