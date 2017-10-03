{-# LANGUAGE DeriveGeneric #-}
module Colorless.Ast
  ( Ast(..)
  , ToAst(..)
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
import qualified Data.Map as Map
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Aeson
import Data.Map (Map)
import Data.Int
import Data.Word
import GHC.Generics (Generic)

import Colorless.Types

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

class ToAst a where
  toAst :: a -> Ast

instance ToAst Bool where
  toAst b = Ast'Const (Const'Bool b)

instance ToAst Text where
  toAst s = Ast'Const (Const'String s)

instance ToAst Int8 where
  toAst = num

instance ToAst Int16 where
  toAst = num

instance ToAst Int32 where
  toAst = num

instance ToAst Int64 where
  toAst = num

instance ToAst Word8 where
  toAst = num

instance ToAst Word16 where
  toAst = num

instance ToAst Word32 where
  toAst = num

instance ToAst Word64 where
  toAst = num

instance ToAst Float where
  toAst = num

instance ToAst Double where
  toAst = num

num :: (Num a, ToJSON a) => a -> Ast
num n = case toJSON n of
  Number a -> Ast'Const (Const'Number a)
  _ -> error "should never reach here"

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
    <|> (Ast'Const <$> parseJSON v)

instance ToJSON Ast where
  toJSON = \case
    Ast'Ref a -> toJSON a
    Ast'If a -> toJSON a
    Ast'Get a -> toJSON a
    Ast'Define a -> toJSON a
    Ast'Lambda a -> toJSON a
    Ast'List a -> toJSON a
    Ast'Tuple a -> toJSON a
    Ast'Begin a -> toJSON a
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
  toJSON List{list} = toJSON $ "list" : map toJSON list

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

data Begin = Begin
  { vals :: [Ast]
  } deriving (Show, Eq)

instance FromJSON Begin where
  parseJSON (Array arr) = case V.toList arr of
    "begin":xs -> Begin <$> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Begin where
  toJSON Begin{vals} = toJSON $ "begin" : map toJSON vals

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
