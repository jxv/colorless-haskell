module Colorless.Client.Expr
  ( Expr
  , Stmt
  , stmt

  , begin
  , def
  , iF

  , eq
  , neq
  , add
  , sub
  , mul
  , divide

  , bool
  , string
  , i8
  , i16
  , i32
  , i64
  , u8
  , u16
  , u32
  , u64

  , call
  , (-<)

  , fn0
  , fn1
  , fn2
  , fn3
  , fn4
  , fn5
  , fn6
  , fn7
  , fn8
  , fn9
  , fn10
  , fn11
  , fn12
  , fn13
  , fn14
  , fn15
  , fn16
  , fn17
  , fn18
  , fn19
  , fn20
  , fn21
  , fn22
  , fn23
  , fn24
  , fn25
  , fn26
  , fn27
  , fn28
  , fn29
  , fn30
  , fn31
  , fn32

  , unsafeExpr
  , unsafeRef
  , unsafeStmt
  ) where

import qualified Data.Vector as V
import Data.Aeson (Value(..), object, (.=), ToJSON(..))
import Data.Word
import Data.Int

import qualified Data.Text as T
import Colorless.Types

-- Don't export constructors
newtype Expr a = Expr Value
  deriving (Show, Eq)

unsafeExpr :: Value -> Expr a
unsafeExpr = Expr

unsafeRef :: Symbol -> Expr a
unsafeRef (Symbol s) = Expr $ object [ "@" .= s ]

instance ToJSON (Expr a) where
  toJSON (Expr v) = v

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
  , ret = unsafeRef symbol
  }

-- ToJSON constraint is to prevent unevaluated functions
stmt :: ToJSON a => Expr a -> Stmt (Expr a)
stmt expr = Stmt [toJSON expr] expr

begin :: Stmt (Expr b) -> Expr b
begin (Stmt s _) = Expr $ toJSON $ "begin" : s

--

iF :: Expr Bool -> Expr a -> Expr a -> Expr a
iF cond t f = Expr $ toJSON ["if", toJSON cond, toJSON t, toJSON f]

eq :: Expr a -> Expr a -> Expr Bool
eq x y = Expr $ toJSON ["==", toJSON x, toJSON y]

neq :: Expr a -> Expr a -> Expr Bool
neq x y = Expr $ toJSON ["!=", toJSON x, toJSON y]

add :: Num a => Expr a -> Expr a -> Expr a
add x y = Expr $ toJSON ["+", toJSON x, toJSON y]

sub :: Num a => Expr a -> Expr a -> Expr a
sub x y = Expr $ toJSON ["-", toJSON x, toJSON y]

mul :: Num a => Expr a -> Expr a -> Expr a
mul x y = Expr $ toJSON ["*", toJSON x, toJSON y]

divide :: Num a => Expr a -> Expr a -> Expr a
divide x y = Expr $ toJSON ["/", toJSON x, toJSON y]

--

bool :: Bool -> Expr Bool
bool = Expr . toJSON

string :: T.Text -> Expr T.Text
string = Expr . toJSON

i8 :: Int8 -> Expr Int8
i8 = Expr . toJSON

i16 :: Int16 -> Expr Int16
i16 = Expr . toJSON

i32 :: Int32 -> Expr Int32
i32 = Expr . toJSON

i64 :: Int64 -> Expr Int64
i64 = Expr . toJSON

u8 :: Word8 -> Expr Word8
u8 = Expr . toJSON

u16 :: Word16 -> Expr Word16
u16 = Expr . toJSON

u32 :: Word32 -> Expr Word32
u32 = Expr . toJSON

u64 :: Word64 -> Expr Word64
u64 = Expr . toJSON

--

call :: ToJSON b => Expr (b -> Expr a) -> b -> Expr a
call f x = case toJSON x of
  Array v -> Expr $ toJSON $ (toJSON f) : V.toList v
  o -> Expr $ toJSON $ [toJSON f, o]

(-<) :: ToJSON b => Expr (b -> Expr a) -> b -> Expr a
f -< x = call f x

fn0
  :: Expr a
  -> Expr (() -> Expr a)
fn0 f = Expr . toJSON $
  [ "fn"
  , toJSON ([] :: [Value])
  , toJSON f
  ]

fn1
  :: Symbol
  -> (Expr t1 -> Expr a)
  -> Expr ((Expr t1) -> Expr a)
fn1 s1@(Symbol t1) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1]
  , toJSON $ f (unsafeRef s1)
  ]

fn2
  :: Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr a)
  -> Expr ((Expr t1, Expr t2) -> Expr a)
fn2 s1@(Symbol t1) s2@(Symbol t2) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2)
  ]

fn3
  :: Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3) -> Expr a)
fn3 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3)
  ]

fn4
  :: Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4) -> Expr a)
fn4 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4)
  ]

fn5
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5) -> Expr a)
fn5 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5)
  ]

fn6
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6) -> Expr a)
fn6 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6)
  ]

fn7
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7) -> Expr a)
fn7 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7)
  ]

fn8
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8) -> Expr a)
fn8 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8)
  ]

fn9
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9) -> Expr a)
fn9 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9)
  ]

fn10
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10) -> Expr a)
fn10 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10)
  ]

fn11
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11) -> Expr a)
fn11 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11)
  ]

fn12
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12) -> Expr a)
fn12 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12)
  ]

fn13
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13) -> Expr a)
fn13 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13)
  ]

fn14
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14) -> Expr a)
fn14 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14)
  ]

fn15
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15) -> Expr a)
fn15 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15)
  ]

fn16
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16) -> Expr a)
fn16 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16)
  ]

fn17
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17) -> Expr a)
fn17 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17)
  ]

fn18
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18) -> Expr a)
fn18 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18)
  ]

fn19
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19) -> Expr a)
fn19 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19)
  ]

fn20
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20) -> Expr a)
fn20 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20)
  ]

fn21
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21) -> Expr a)
fn21 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21)
  ]

fn22
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22) -> Expr a)
fn22 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22)
  ]

fn23
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23) -> Expr a)
fn23 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23)
  ]

fn24
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24) -> Expr a)
fn24 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24)
  ]

fn25
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25) -> Expr a)
fn25 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25)
  ]

fn26
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26) -> Expr a)
fn26 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26)
  ]

fn27
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27) -> Expr a)
fn27 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27)
  ]

fn28
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28) -> Expr a)
fn28 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) s28@(Symbol t28) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27, toJSON t28]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28)
  ]

fn29
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29) -> Expr a)
fn29 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) s28@(Symbol t28) s29@(Symbol t29) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27, toJSON t28, toJSON t29]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29)
  ]

fn30
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30) -> Expr a)
fn30 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) s28@(Symbol t28) s29@(Symbol t29) s30@(Symbol t30) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27, toJSON t28, toJSON t29, toJSON t30]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30)
  ]

fn31
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31) -> Expr a)
fn31 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) s28@(Symbol t28) s29@(Symbol t29) s30@(Symbol t30) s31@(Symbol t31) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27, toJSON t28, toJSON t29, toJSON t30, toJSON t31]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30) (unsafeRef s31)
  ]

fn32
  :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr t32 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31, Expr t32) -> Expr a)
fn32 s1@(Symbol t1) s2@(Symbol t2) s3@(Symbol t3) s4@(Symbol t4) s5@(Symbol t5) s6@(Symbol t6) s7@(Symbol t7) s8@(Symbol t8) s9@(Symbol t9) s10@(Symbol t10) s11@(Symbol t11) s12@(Symbol t12) s13@(Symbol t13) s14@(Symbol t14) s15@(Symbol t15) s16@(Symbol t16) s17@(Symbol t17) s18@(Symbol t18) s19@(Symbol t19) s20@(Symbol t20) s21@(Symbol t21) s22@(Symbol t22) s23@(Symbol t23) s24@(Symbol t24) s25@(Symbol t25) s26@(Symbol t26) s27@(Symbol t27) s28@(Symbol t28) s29@(Symbol t29) s30@(Symbol t30) s31@(Symbol t31) s32@(Symbol t32) f = Expr . toJSON $
  [ "fn"
  , toJSON [toJSON t1, toJSON t2, toJSON t3, toJSON t4, toJSON t5, toJSON t6, toJSON t7, toJSON t8, toJSON t9, toJSON t10, toJSON t11, toJSON t12, toJSON t13, toJSON t14, toJSON t15, toJSON t16, toJSON t17, toJSON t18, toJSON t19, toJSON t20, toJSON t21, toJSON t22, toJSON t23, toJSON t24, toJSON t25, toJSON t26, toJSON t27, toJSON t28, toJSON t29, toJSON t30, toJSON t31, toJSON t32]
  , toJSON $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30) (unsafeRef s31) (unsafeRef s32)
  ]
