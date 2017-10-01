{-# LANGUAGE ScopedTypeVariables #-}
module Colorless.Client.Expr
  ( Expr
  , Stmt
  , Path
  , ToArgs
  , stmt
  --
  , begin
  , def
  , defRec
  , iF
  , get
  , dot
  , (<.>)
  --
  , eq
  , neq
  , add
  , sub
  , mul
  , divide
  --
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
  , f32
  , f64
  --
  , option
  , list
  , eitheR
  --
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  , tuple8
  , tuple9
  , tuple10
  , tuple11
  , tuple12
  , tuple13
  , tuple14
  , tuple15
  , tuple16
  , tuple17
  , tuple18
  , tuple19
  , tuple20
  , tuple21
  , tuple22
  , tuple23
  , tuple24
  , tuple25
  , tuple26
  , tuple27
  , tuple28
  , tuple29
  , tuple30
  , tuple31
  , tuple32
  --
  , call
  , (-<)
  --
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
  --
  , unsafeExpr
  , unsafeRef
  , unsafeStmt
  , unsafePath
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Proxy
import Data.Word
import Data.Int

import qualified Colorless.Ast as Ast

import Colorless.Ast (Ast(..), ToAst(..))
import Colorless.Types

-- Don't export constructors
newtype Expr a = Expr Ast
  deriving (Show, Eq)

instance HasType a => HasType (Expr a) where
  getType p = getType (cvt p)
    where
      cvt :: Proxy (Expr a) -> Proxy a
      cvt _ = Proxy

unsafeExpr :: Ast -> Expr a
unsafeExpr = Expr

unsafeRef :: Symbol -> Expr a
unsafeRef s = Expr (Ast'Ref $ Ast.Ref s)

instance ToAst (Expr a) where
  toAst (Expr v) = v

-- Don't export constructor
data Stmt a = Stmt
  { stmts :: [Ast]
  , ret :: a
  } deriving (Show, Eq)

unsafeStmt :: [Ast] -> a -> Stmt a
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
def symbol (Expr ast) = Stmt
  { stmts = [Ast'Define $ Ast.Define symbol ast]
  , ret = unsafeRef symbol
  }

defRec :: Symbol -> (Expr a -> Expr a) -> Stmt (Expr a)
defRec symbol f = Stmt
  { stmts = [Ast'Define $ Ast.Define symbol ast]
  , ret = fn
  }
  where
    fn = unsafeRef symbol
    Expr ast = f fn

-- ToAst constraint is to prevent unevaluated functions
stmt :: ToAst a => Expr a -> Stmt (Expr a)
stmt expr = Stmt [toAst expr] expr

begin :: Stmt (Expr b) -> Expr b
begin (Stmt s _) = Expr (Ast'Begin $ Ast.Begin s)

--

iF :: Expr Bool -> Expr a -> Expr a -> Expr a
iF cond t f = Expr (Ast'If $ Ast.If (toAst cond) (toAst t) (toAst f))

--

newtype Path f = Path [T.Text]
  deriving (Show, Eq)

dot :: Path (a -> b) -> Path (b -> c) -> Path (a -> c)
dot (Path p1) (Path p2) = Path (p1 ++ p2)

(<.>) :: Path (a -> b) -> Path (b -> c) -> Path (a -> c)
(<.>) = dot

unsafePath :: [T.Text] -> Path f
unsafePath = Path

get :: Path (a -> b) -> Expr a -> Expr b
get (Path path) expr = Expr $ Ast'Get $ Ast.Get path (toAst expr)

--

eq :: Expr a -> Expr a -> Expr Bool
eq x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "==") [toAst x, toAst y])

neq :: Expr a -> Expr a -> Expr Bool
neq x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "!=") [toAst x, toAst y])

add :: Num a => Expr a -> Expr a -> Expr a
add x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "+") [toAst x, toAst y])

sub :: Num a => Expr a -> Expr a -> Expr a
sub x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "-") [toAst x, toAst y])

mul :: Num a => Expr a -> Expr a -> Expr a
mul x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "*") [toAst x, toAst y])

divide :: Num a => Expr a -> Expr a -> Expr a
divide x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "/") [toAst x, toAst y])

--

bool :: Bool -> Expr Bool
bool = Expr . toAst

string :: T.Text -> Expr T.Text
string = Expr . toAst

i8 :: Int8 -> Expr Int8
i8 = Expr . toAst

i16 :: Int16 -> Expr Int16
i16 = Expr . toAst

i32 :: Int32 -> Expr Int32
i32 = Expr . toAst

i64 :: Int64 -> Expr Int64
i64 = Expr . toAst

u8 :: Word8 -> Expr Word8
u8 = Expr . toAst

u16 :: Word16 -> Expr Word16
u16 = Expr . toAst

u32 :: Word32 -> Expr Word32
u32 = Expr . toAst

u64 :: Word64 -> Expr Word64
u64 = Expr . toAst

f32 :: Float -> Expr Float
f32 = Expr . toAst

f64 :: Double -> Expr Double
f64 = Expr . toAst

--

list :: [Expr a] -> Expr [a]
list = Expr . Ast'List . Ast.List . map toAst

option :: Maybe (Expr a) -> Expr (Maybe a)
option = \case
  Nothing -> Expr (Ast'Const Const'Null)
  Just (Expr v) -> Expr v

eitheR :: Either (Expr a) (Expr b) -> Expr (Either a b)
eitheR = \case
  Left expr -> Expr $ Ast'Enumeral $ Ast.Enumeral "Left" $ Just $ Map.fromList [("left", toAst expr)]
  Right expr -> Expr $ Ast'Enumeral $ Ast.Enumeral "Right" $ Just $ Map.fromList [("right", toAst expr)]

--

tuple2 :: Expr t1 -> Expr t2 -> Expr (t1, t2)
tuple2 t1 t2 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2]

tuple3 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr (t1, t2, t3)
tuple3 t1 t2 t3 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3]

tuple4 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4]

tuple5 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr (t1, t2, t3, t4, t5)
tuple5 t1 t2 t3 t4 t5 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5]

tuple6 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr (t1, t2, t3, t4, t5, t6)
tuple6 t1 t2 t3 t4 t5 t6 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6]

tuple7 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr (t1, t2, t3, t4, t5, t6, t7)
tuple7 t1 t2 t3 t4 t5 t6 t7 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7]

tuple8 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8)
tuple8 t1 t2 t3 t4 t5 t6 t7 t8 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8]

tuple9 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9)
tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9]

tuple10 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10]

tuple11 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
tuple11 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11]

tuple12 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
tuple12 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12]

tuple13 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
tuple13 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13]

tuple14 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
tuple14 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14]

tuple15 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
tuple15 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15]

tuple16 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
tuple16 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16]

tuple17 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
tuple17 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17]

tuple18 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
tuple18 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18]

tuple19 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
tuple19 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19]

tuple20 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
tuple20 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20]

tuple21 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
tuple21 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21]

tuple22 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
tuple22 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22]

tuple23 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23)
tuple23 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23]

tuple24 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24)
tuple24 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24]

tuple25 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25)
tuple25 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25]

tuple26 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26)
tuple26 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26]

tuple27 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27)
tuple27 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27]

tuple28 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28)
tuple28 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28]

tuple29 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29)
tuple29 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29]

tuple30 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30)
tuple30 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30]

tuple31 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31)
tuple31 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31]

tuple32 :: Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr t32 -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32)
tuple32 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31, toAst t32]

--

class ToArgs a where
  toArgs :: a -> [Ast]

instance ToArgs () where
  toArgs _ = []

instance ToAst a => ToArgs (Expr a) where
  toArgs x = [toAst x]

instance (ToAst t1, ToAst t2) => ToArgs (t1, t2) where
  toArgs (t1, t2) = [toAst t1, toAst t2]

instance (ToAst t1, ToAst t2, ToAst t3) => ToArgs (t1, t2, t3) where
  toArgs (t1, t2, t3) = [toAst t1, toAst t2, toAst t3]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4) => ToArgs (t1, t2, t3, t4) where
  toArgs (t1, t2, t3, t4) = [toAst t1, toAst t2, toAst t3, toAst t4]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5) => ToArgs (t1, t2, t3, t4, t5) where
  toArgs (t1, t2, t3, t4, t5) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6) => ToArgs (t1, t2, t3, t4, t5, t6) where
  toArgs (t1, t2, t3, t4, t5, t6) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7) => ToArgs (t1, t2, t3, t4, t5, t6, t7) where
  toArgs (t1, t2, t3, t4, t5, t6, t7) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30, ToAst t31) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31]

instance (ToAst t1, ToAst t2, ToAst t3, ToAst t4, ToAst t5, ToAst t6, ToAst t7, ToAst t8, ToAst t9, ToAst t10, ToAst t11, ToAst t12, ToAst t13, ToAst t14, ToAst t15, ToAst t16, ToAst t17, ToAst t18, ToAst t19, ToAst t20, ToAst t21, ToAst t22, ToAst t23, ToAst t24, ToAst t25, ToAst t26, ToAst t27, ToAst t28, ToAst t29, ToAst t30, ToAst t31, ToAst t32) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31, toAst t32]

{-
var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var toArgs = n => {
  if (n < 2) {
    return '';
  }

  var l = ['instance (ToAst t1'];
  for (var i = 1; i < n; i++) {
    l = l.concat([', ToAst t', i + 1])
  }
  l = l.concat([') => ToArgs (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1])
  }
  l = l.concat([') where\n']);

  l = l.concat(['  toArgs (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') = [toAst t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', toAst t', i + 1]);
  }
  l = l.concat([']\n\n']);

  return l.join('');
};
-}

call :: ToArgs b => Expr (b -> Expr a) -> b -> Expr a
call f args = Expr $ Ast'FnCall $ Ast.FnCall (toAst f) (toArgs args)

(-<) :: ToArgs b => Expr (b -> Expr a) -> b -> Expr a
f -< x = call f x

fn0
  :: Expr a
  -> Expr (() -> Expr a)
fn0 f = Expr $ Ast'Lambda $ Ast.Lambda
  []
  (toAst f)

fn1
  :: (HasType t1)
  => Symbol
  -> (Expr t1 -> Expr a)
  -> Expr ((Expr t1) -> Expr a)
fn1 s1 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f))]
  (toAst $ f (unsafeRef s1))
  where
    p1 :: (t1 -> a) -> Proxy t1
    p1 _ = Proxy

fn2
  :: (HasType t1, HasType t2)
  => Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr a)
  -> Expr ((Expr t1, Expr t2) -> Expr a)
fn2 s1 s2 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2))
  where
    p1 :: (t1 -> t2 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> a) -> Proxy t2
    p2 _ = Proxy

fn3
  :: (HasType t1, HasType t2, HasType t3)
  => Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3) -> Expr a)
fn3 s1 s2 s3 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3))
  where
    p1 :: (t1 -> t2 -> t3 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> a) -> Proxy t3
    p3 _ = Proxy

fn4
  :: (HasType t1, HasType t2, HasType t3, HasType t4)
  => Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4) -> Expr a)
fn4 s1 s2 s3 s4 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> a) -> Proxy t4
    p4 _ = Proxy

fn5
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5) -> Expr a)
fn5 s1 s2 s3 s4 s5 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> a) -> Proxy t5
    p5 _ = Proxy

fn6
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6) -> Expr a)
fn6 s1 s2 s3 s4 s5 s6 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a) -> Proxy t6
    p6 _ = Proxy

fn7
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7) -> Expr a)
fn7 s1 s2 s3 s4 s5 s6 s7 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a) -> Proxy t7
    p7 _ = Proxy

fn8
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8) -> Expr a)
fn8 s1 s2 s3 s4 s5 s6 s7 s8 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a) -> Proxy t8
    p8 _ = Proxy

fn9
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9) -> Expr a)
fn9 s1 s2 s3 s4 s5 s6 s7 s8 s9 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a) -> Proxy t9
    p9 _ = Proxy

fn10
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10) -> Expr a)
fn10 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> a) -> Proxy t10
    p10 _ = Proxy

fn11
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11) -> Expr a)
fn11 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> a) -> Proxy t11
    p11 _ = Proxy

fn12
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12) -> Expr a)
fn12 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> a) -> Proxy t12
    p12 _ = Proxy

fn13
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13) -> Expr a)
fn13 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> a) -> Proxy t13
    p13 _ = Proxy

fn14
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14) -> Expr a)
fn14 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> a) -> Proxy t14
    p14 _ = Proxy

fn15
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15) -> Expr a)
fn15 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> a) -> Proxy t15
    p15 _ = Proxy

fn16
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16) -> Expr a)
fn16 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> a) -> Proxy t16
    p16 _ = Proxy

fn17
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17) -> Expr a)
fn17 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> a) -> Proxy t17
    p17 _ = Proxy

fn18
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18) -> Expr a)
fn18 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> a) -> Proxy t18
    p18 _ = Proxy

fn19
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19) -> Expr a)
fn19 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> a) -> Proxy t19
    p19 _ = Proxy

fn20
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20) -> Expr a)
fn20 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> a) -> Proxy t20
    p20 _ = Proxy

fn21
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21) -> Expr a)
fn21 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> a) -> Proxy t21
    p21 _ = Proxy

fn22
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22) -> Expr a)
fn22 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> a) -> Proxy t22
    p22 _ = Proxy

fn23
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23) -> Expr a)
fn23 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> a) -> Proxy t23
    p23 _ = Proxy

fn24
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24) -> Expr a)
fn24 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> a) -> Proxy t24
    p24 _ = Proxy

fn25
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25) -> Expr a)
fn25 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> a) -> Proxy t25
    p25 _ = Proxy

fn26
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26) -> Expr a)
fn26 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> a) -> Proxy t26
    p26 _ = Proxy

fn27
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27) -> Expr a)
fn27 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> a) -> Proxy t27
    p27 _ = Proxy

fn28
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28) -> Expr a)
fn28 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f)), (s28, getType (p28 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t27
    p27 _ = Proxy
    p28 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> a) -> Proxy t28
    p28 _ = Proxy

fn29
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29) -> Expr a)
fn29 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f)), (s28, getType (p28 f)), (s29, getType (p29 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t27
    p27 _ = Proxy
    p28 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t28
    p28 _ = Proxy
    p29 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> a) -> Proxy t29
    p29 _ = Proxy

fn30
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30) -> Expr a)
fn30 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f)), (s28, getType (p28 f)), (s29, getType (p29 f)), (s30, getType (p30 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t27
    p27 _ = Proxy
    p28 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t28
    p28 _ = Proxy
    p29 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t29
    p29 _ = Proxy
    p30 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> a) -> Proxy t30
    p30 _ = Proxy

fn31
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31) -> Expr a)
fn31 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f)), (s28, getType (p28 f)), (s29, getType (p29 f)), (s30, getType (p30 f)), (s31, getType (p31 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30) (unsafeRef s31))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t27
    p27 _ = Proxy
    p28 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t28
    p28 _ = Proxy
    p29 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t29
    p29 _ = Proxy
    p30 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t30
    p30 _ = Proxy
    p31 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> a) -> Proxy t31
    p31 _ = Proxy

fn32
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31, HasType t32)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr t32 -> Expr a)
  -> Expr ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31, Expr t32) -> Expr a)
fn32 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f)), (s3, getType (p3 f)), (s4, getType (p4 f)), (s5, getType (p5 f)), (s6, getType (p6 f)), (s7, getType (p7 f)), (s8, getType (p8 f)), (s9, getType (p9 f)), (s10, getType (p10 f)), (s11, getType (p11 f)), (s12, getType (p12 f)), (s13, getType (p13 f)), (s14, getType (p14 f)), (s15, getType (p15 f)), (s16, getType (p16 f)), (s17, getType (p17 f)), (s18, getType (p18 f)), (s19, getType (p19 f)), (s20, getType (p20 f)), (s21, getType (p21 f)), (s22, getType (p22 f)), (s23, getType (p23 f)), (s24, getType (p24 f)), (s25, getType (p25 f)), (s26, getType (p26 f)), (s27, getType (p27 f)), (s28, getType (p28 f)), (s29, getType (p29 f)), (s30, getType (p30 f)), (s31, getType (p31 f)), (s32, getType (p32 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2) (unsafeRef s3) (unsafeRef s4) (unsafeRef s5) (unsafeRef s6) (unsafeRef s7) (unsafeRef s8) (unsafeRef s9) (unsafeRef s10) (unsafeRef s11) (unsafeRef s12) (unsafeRef s13) (unsafeRef s14) (unsafeRef s15) (unsafeRef s16) (unsafeRef s17) (unsafeRef s18) (unsafeRef s19) (unsafeRef s20) (unsafeRef s21) (unsafeRef s22) (unsafeRef s23) (unsafeRef s24) (unsafeRef s25) (unsafeRef s26) (unsafeRef s27) (unsafeRef s28) (unsafeRef s29) (unsafeRef s30) (unsafeRef s31) (unsafeRef s32))
  where
    p1 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t2
    p2 _ = Proxy
    p3 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t3
    p3 _ = Proxy
    p4 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t4
    p4 _ = Proxy
    p5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t5
    p5 _ = Proxy
    p6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t6
    p6 _ = Proxy
    p7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t7
    p7 _ = Proxy
    p8 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t8
    p8 _ = Proxy
    p9 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t9
    p9 _ = Proxy
    p10 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t10
    p10 _ = Proxy
    p11 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t11
    p11 _ = Proxy
    p12 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t12
    p12 _ = Proxy
    p13 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t13
    p13 _ = Proxy
    p14 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t14
    p14 _ = Proxy
    p15 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t15
    p15 _ = Proxy
    p16 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t16
    p16 _ = Proxy
    p17 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t17
    p17 _ = Proxy
    p18 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t18
    p18 _ = Proxy
    p19 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t19
    p19 _ = Proxy
    p20 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t20
    p20 _ = Proxy
    p21 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t21
    p21 _ = Proxy
    p22 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t22
    p22 _ = Proxy
    p23 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t23
    p23 _ = Proxy
    p24 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t24
    p24 _ = Proxy
    p25 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t25
    p25 _ = Proxy
    p26 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t26
    p26 _ = Proxy
    p27 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t27
    p27 _ = Proxy
    p28 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t28
    p28 _ = Proxy
    p29 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t29
    p29 _ = Proxy
    p30 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t30
    p30 _ = Proxy
    p31 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t31
    p31 _ = Proxy
    p32 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> a) -> Proxy t32
    p32 _ = Proxy

{-
var tuples = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var fn = n => {
  if (n < 1) {
    return '';
  }
  var l = ['fn', n, '\n'];

  l = l.concat(['  :: (HasType t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', HasType t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['  => Symbol']);
  for (var i = 1; i < n; i++) {
    l = l.concat([' -> Symbol']);
  }
  l = l.concat(['\n']);

  l = l.concat(['  -> (Expr t1'])
  for (var i = 1; i < n; i++) {
    l = l.concat([' -> Expr t', i + 1]);
  }
  l = l.concat([' -> Expr a)\n']);

  l = l.concat(['  -> Expr ((Expr t1'])
  for (var i = 1; i < n; i++) {
    l = l.concat([', Expr t', i + 1]);
  }
  l = l.concat([') -> Expr a)\n']);

  l = l.concat(['fn', n]);
  for (var i = 0; i < n; i++) {
    l = l.concat([' s', i + 1]);
  }
  l = l.concat([' f = Expr $ Ast\'Lambda $ Ast.Lambda\n'])

  l = l.concat(['  [(s1, getType (p1 f))']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', (s', i + 1, ', getType (p', i + 1,' f))']);
  }
  l = l.concat([']\n']);

  l = l.concat(['  (toAst $ f']);
  for (var i = 0; i < n; i++) {
    l = l.concat([' (unsafeRef s', i + 1, ')']);
  }
  l = l.concat([')\n']);

  l = l.concat(['  where\n']);
  var f = ['(t1'];
  for (var i = 1; i < n; i++) {
    f  = f.concat([' -> t', i + 1]);
  }
  f = f.concat([' -> a)']).join('');
  for (var i = 0; i < n; i++) {
    l = l.concat([
      '    p', i + 1, ' :: ', f, ' -> Proxy t', i + 1, '\n',
      '    p', i + 1, ' _ = Proxy\n',
    ]);
  }
  l = l.concat(['\n']);

  return l.join('');
};
-}
