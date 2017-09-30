module Colorless.Client.Expr
  ( hello'
  ) where

import qualified Data.Vector as V
import Data.Aeson (Value(..), object, (.=), ToJSON(..))
import Data.Word
import Data.Int

import Colorless.Types

-----------

-- Don't export constructors
data Expr a
  = Expr Value
  | Ref Symbol
  deriving (Show, Eq)

unsafeExpr :: Value -> Expr a
unsafeExpr = Expr

unsafeRef :: Symbol -> Expr a
unsafeRef = Ref

instance ToJSON (Expr a) where
  toJSON (Expr v) = v
  toJSON (Ref (Symbol s)) = object [ "@" .= s ]

-- Don't export constructor
data Stmt a = Stmt
  { stmts :: [Value]
  , ret :: a
  } deriving (Show, Eq)

unsafeStmt :: [Value] -> a -> Stmt a
unsafeStmt = Stmt

instance Functor Stmt where
  fmap f x = x { ret = f (ret x) }

instance Applicative Stmt where
  pure a = Stmt [] a
  (Stmt s0 f) <*> (Stmt s1 x) = Stmt (s0 ++ s1) (f x)

instance Monad Stmt where
  return a = Stmt [] a
  x >>= f = let
    y = f (ret x)
    in Stmt (stmts x ++ stmts y) (ret y)

def :: Symbol -> Expr a -> Stmt (Expr a)
def symbol@(Symbol sym) expr = Stmt
  { stmts = [toJSON ["def", toJSON sym, toJSON expr]]
  , ret = Ref symbol
  }

-- ToJSON constraint is to prevent unevaluated functions
stmt :: ToJSON a => Expr a -> Stmt (Expr a)
stmt expr = Stmt [toJSON expr] expr

begin :: Stmt (Expr b) -> Expr b
begin (Stmt s _) = Expr $ toJSON $ "begin" : s

--

eq :: Expr a -> Expr a -> Expr Bool
eq x y = Expr $ toJSON ["==", toJSON x, toJSON y]

add :: Num a => Expr a -> Expr a -> Expr a
add x y = Expr $ toJSON ["+", toJSON x, toJSON y]

iF :: Expr Bool -> Expr a -> Expr a -> Expr a
iF cond t f = Expr $ toJSON ["if", toJSON cond, toJSON t, toJSON f]

--

i8 :: Int8 -> Expr Int8
i8 = Expr . toJSON

i16 :: Int16 -> Expr Int16
i16 = Expr . toJSON

u8 :: Word8 -> Expr Word8
u8 = Expr . toJSON

u16 :: Word16 -> Expr Word16
u16 = Expr . toJSON

--

call :: ToJSON b => Expr (b -> Expr a) -> b -> Expr a
call f x = case toJSON x of
  Array v -> Expr $ toJSON $ (toJSON f) : V.toList v
  o -> Expr $ toJSON $ [toJSON f, o]

(-<) :: ToJSON b => Expr (b -> Expr a) -> b -> Expr a
f -< x = call f x

fn1
  :: Symbol
  -> (Expr t1 -> Expr a)
  -> Expr (Expr t1 -> Expr a)
fn1 s1@(Symbol t1) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1]
  , toJSON $ f (Ref s1)
  ]

fn2
  :: Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr a)
  -> Expr ((Expr t1, Expr t2) -> Expr a)
fn2 s1@(Symbol t1) s2@(Symbol t2) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2]
  , toJSON $ f (Ref s1) (Ref s2)
  ]

-- goodbye :: Goodbye -> Expr ()
-- goodbye v = Expr . object [ "n" .= "Goodbye", "m" .= v ]

-- q1 :: Expr () a -> Query a
-- q2 :: Expr a' a -> (a' -> Expr b' b) -> Query (a, b)
-- q3 :: Expr a' a -> (a' -> Expr b' b) -> (b' -> Expr () c) -> Query (a, b, c)
-- q4 :: Expr a' a -> (a' -> Expr b' b) -> (b' -> Expr c' c) -> (c' -> Expr () d) -> Query (a, b, c, d)

hello :: Stmt (Expr Word8)
hello = def "hello" (iF (eq (i8 1) (i8 2)) (u8 1) (u8 2))

hello' :: Expr Word8
hello' = begin $ do
  ref <- hello
  z <- def "k" $ fn2 "x" "y" (\x y -> add x y)
  stmt $ add ref (z -< (u8 10, u8 20))
