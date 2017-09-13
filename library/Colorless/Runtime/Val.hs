{-# LANGUAGE DeriveGeneric #-}
module Colorless.Runtime.Val
  ( Val(..)
  , ApiVal(..)
  , Wrap(..)
  , Struct(..)
  , Enumerator(..)
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

import Colorless.Runtime.Types

data Val
  = Val'Const Const
  | Val'ApiVal ApiVal
  | Val'List [Val]
  deriving (Show, Eq)

instance ToJSON Val where
  toJSON = \case
    Val'Const c -> toJSON c
    Val'ApiVal v -> toJSON v
    Val'List l -> toJSON l

instance FromJSON Val where
  parseJSON = \case
    Null -> return $ Val'Const Const'Null
    Number n -> return $ Val'Const $ Const'Number n
    String s -> return $ Val'Const $ Const'String s
    Bool b -> return $ Val'Const $ Const'Bool b
    Array arr -> Val'List <$> (mapM parseJSON $ V.toList arr)
    v@Object{} -> Val'ApiVal <$> parseJSON v

data ApiVal
  = ApiVal'Wrap Wrap
  | ApiVal'Struct Struct
  | ApiVal'Enumerator Enumerator
  deriving (Show, Eq)

instance ToJSON ApiVal where
  toJSON = \case
    ApiVal'Wrap w -> toJSON w
    ApiVal'Struct s -> toJSON s
    ApiVal'Enumerator e -> toJSON e

instance FromJSON ApiVal where
  parseJSON v =
    (ApiVal'Wrap <$> parseJSON v) <|>
    (ApiVal'Enumerator <$> parseJSON v) <|>
    (ApiVal'Struct <$> parseJSON v)

data Wrap = Wrap
  { w :: Const
  } deriving (Show, Eq, Generic)

instance ToJSON Wrap
instance FromJSON Wrap

data Struct = Struct
  { m :: Map MemberName Val
  } deriving (Show, Eq, Generic)

instance ToJSON Struct where
  toJSON Struct{m} = toJSON m

instance FromJSON Struct where
  parseJSON v = Struct <$> parseJSON v

data Enumerator = Enumerator
  { tag :: EnumeratorName
  , m :: Maybe (Map MemberName Val)
  } deriving (Show, Eq, Generic)

instance ToJSON Enumerator where
  toJSON Enumerator{tag,m} = object $ [ "tag" .= tag ] ++ case m of
    Nothing -> []
    Just m' -> concatMap (\(MemberName k,v) -> [ k .= v ]) (Map.toList m')

instance FromJSON Enumerator where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    let tagless = HML.delete "tag" o
    if HML.size o == 1
      then pure $ Enumerator tag Nothing
      else Enumerator tag <$> (Just <$> parseJSON (Object tagless))
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
  toVal = intToVal

instance ToVal Int16 where
  toVal = intToVal

instance ToVal Int32 where
  toVal = intToVal

instance ToVal Int64 where
  toVal = intToVal

instance ToVal Word where
  toVal = intToVal

instance ToVal Word8 where
  toVal = intToVal

instance ToVal Word16 where
  toVal = intToVal

instance ToVal Word32 where
  toVal = intToVal

instance ToVal Word64 where
  toVal = intToVal

instance ToVal Float where
    toVal f = Val'Const $ Const'Number $ fromFloatDigits f

instance ToVal Double where
    toVal d = Val'Const $ Const'Number $ fromFloatDigits d

instance ToVal a => ToVal (Maybe a) where
  toVal m = Val'ApiVal $ ApiVal'Enumerator $ case m of
    Nothing -> Enumerator "None" Nothing
    Just s -> Enumerator "Some" (Just $ Map.singleton "some" (toVal s))

instance (ToVal a, ToVal b) => ToVal (Either a b) where
  toVal m = Val'ApiVal $ ApiVal'Enumerator $ case m of
    Left l -> Enumerator "Left" (Just $ Map.singleton "left" (toVal l))
    Right r -> Enumerator "Right" (Just $ Map.singleton "right" (toVal r))

instance ToVal a => ToVal [a] where
  toVal list = Val'List $ map toVal list

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
  fromVal = intFromVal

instance FromVal Int16 where
  fromVal = intFromVal

instance FromVal Int32 where
  fromVal = intFromVal

instance FromVal Int64 where
  fromVal = intFromVal

instance FromVal Word where
  fromVal = intFromVal

instance FromVal Word8 where
  fromVal = intFromVal

instance FromVal Word16 where
  fromVal = intFromVal

instance FromVal Word32 where
  fromVal = intFromVal

instance FromVal Word64 where
  fromVal = intFromVal

instance FromVal Float where
  fromVal (Val'Const (Const'Number n)) = Just $ toRealFloat n
  fromVal _ = Nothing

instance FromVal Double where
  fromVal (Val'Const (Const'Number n)) = Just $ toRealFloat n
  fromVal _ = Nothing

instance FromVal a => FromVal (Maybe a) where
  fromVal (Val'ApiVal (ApiVal'Enumerator (Enumerator tag m))) = case (tag,m) of
    ("Some",Just m') -> fromVal <$> Map.lookup "some" m'
    ("None",Nothing) -> Just Nothing
    _ -> Nothing
  fromVal _ = Nothing

instance (FromVal a, FromVal b) => FromVal (Either a b) where
  fromVal (Val'ApiVal (ApiVal'Enumerator (Enumerator tag m))) = case (tag,m) of
    ("Left",Just m') -> Map.lookup "left" m' >>= \l -> Left <$> fromVal l
    ("Right",Just m') -> Map.lookup "right" m' >>= \r -> Right <$> fromVal r
    _ -> Nothing
  fromVal _ = Nothing

instance FromVal a => FromVal [a] where
  fromVal (Val'List list) = mapM fromVal list
  fromVal _ = Nothing

getMember :: FromVal a => Map MemberName Val -> MemberName -> Maybe a
getMember m n = fromVal =<< Map.lookup n m

fromValFromJson :: (FromVal b) => Value -> Maybe b
fromValFromJson x = fromVal =<< parseMaybe parseJSON x

combineObjects :: Value -> Value -> Value
combineObjects (Object x) (Object y) = Object $ HML.union x y
combineObjects _ _ = error "not objects"
