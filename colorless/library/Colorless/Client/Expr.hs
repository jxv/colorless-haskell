module Colorless.Client.Expr
  ( Expr
  , Stmt
  , Path
  , Fn
  , ToArgs
  , ToExpr(..)
  , exprJSON
  , stmt
  --
  , appendExpr
  , (<:>)
  --
  , begin
  , def
  , defn
  , defnRec
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
  , concaT
  --
  , unit
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
  , tuple2'
  , tuple3'
  , tuple4'
  , tuple5'
  , tuple6'
  , tuple7'
  , tuple8'
  , tuple9'
  , tuple10'
  , tuple11'
  , tuple12'
  , tuple13'
  , tuple14'
  , tuple15'
  , tuple16'
  , tuple17'
  , tuple18'
  , tuple19'
  , tuple20'
  , tuple21'
  , tuple22'
  , tuple23'
  , tuple24'
  , tuple25'
  , tuple26'
  , tuple27'
  , tuple28'
  , tuple29'
  , tuple30'
  , tuple31'
  , tuple32'
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
  , unsafeWrapExpr
  , unsafeStructExpr
  , unsafeEnumeralExpr
  , unsafeStmt
  , unsafePath
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Aeson (toJSON, Value)
import Data.Proxy
import Data.Word
import Data.Int
import Data.Map (Map)

import qualified Colorless.Ast as Ast

import Colorless.Ast (Ast(..), ToAst(..))
import Colorless.Types

exprJSON :: (HasType a, ToAst a) => Expr a -> Value
exprJSON = toJSON . toAst

-- Don't export constructors
data Expr a
  = Expr Ast
  | Expr'WrapCtor
  | Expr'MembersCtor MembersCtor (Map MemberName Ast)

newtype MembersCtor = MembersCtor (Ast -> Map MemberName Ast -> Either (Map MemberName Ast, MembersCtor) Ast)

unsafeWrapExpr :: Expr a
unsafeWrapExpr = Expr'WrapCtor

structCtor :: [MemberName] -> MembersCtor
structCtor [] = error "need members"
structCtor (n:[]) = MembersCtor $ \ast m -> Right (Ast.Ast'Struct $ Ast.Struct $ Map.insert n ast m)
structCtor (n:ns) = MembersCtor $ \ast m -> Left (Map.insert n ast m, structCtor ns)

unsafeStructExpr :: [MemberName] -> Expr a
unsafeStructExpr ns = Expr'MembersCtor (structCtor ns) Map.empty

enumeralCtor :: EnumeralName -> [MemberName] -> MembersCtor
enumeralCtor _ [] = error "need members"
enumeralCtor tag (n:[]) = MembersCtor $ \ast m -> Right (Ast.Ast'Enumeral $ Ast.Enumeral tag $ Just $ Map.insert n ast m)
enumeralCtor tag (n:ns) = MembersCtor $ \ast m -> Left (Map.insert n ast m, enumeralCtor tag ns)

unsafeEnumeralExpr :: EnumeralName -> [MemberName] -> Expr a
unsafeEnumeralExpr tag ns = Expr'MembersCtor (enumeralCtor tag ns) Map.empty

appendExpr :: Expr (a -> b) -> Expr a -> Expr b
appendExpr (Expr'MembersCtor (MembersCtor c) m) (Expr a) = case c a m of
  Left (m', ctor) -> Expr'MembersCtor ctor m'
  Right ast -> Expr ast
appendExpr Expr'WrapCtor (Expr a) = Expr a
appendExpr _ _ = error "cannot not append member"

(<:>) :: Expr (a -> b) -> Expr a -> Expr b
(<:>) = appendExpr

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
  toAst Expr'WrapCtor = error "Unevaluated wrap constructor expression cannot into an AST"
  toAst (Expr'MembersCtor _ _) = error "Unevaluated member constructor expression cannot convert into an AST"

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

def :: HasType a => Symbol -> Expr a -> Stmt (Expr a)
def symbol expr = Stmt
  { stmts = [Ast'Define $ Ast.Define symbol (toAst expr)]
  , ret = unsafeRef symbol
  }

defn :: Symbol -> Expr (Fn a) -> Stmt (Expr (Fn a))
defn symbol expr = Stmt
  { stmts = [Ast'Define $ Ast.Define symbol (toAst expr)]
  , ret = unsafeRef symbol
  }

defnRec :: Symbol -> (Expr (Fn a) -> Expr (Fn a)) -> Stmt (Expr (Fn a))
defnRec symbol f = Stmt
  { stmts = [Ast'Define $ Ast.Define symbol ast]
  , ret = fn
  }
  where
    fn = unsafeRef symbol
    ast = toAst $ f fn

-- HasType constraint is to prevent unevaluated functions
stmt :: HasType a => Expr a -> Stmt (Expr a)
stmt expr = Stmt [toAst expr] expr

begin :: HasType a => Stmt (Expr a) -> Expr a
begin (Stmt s _) = Expr (Ast'Begin $ Ast.Begin s)

--

iF :: HasType a => Expr Bool -> Expr a -> Expr a -> Expr a
iF cond t f = Expr (Ast'If $ Ast.If (toAst cond) (toAst t) (toAst f))

--

newtype Path f = Path [T.Text]
  deriving (Show, Eq)

dot :: (HasType a, HasType b, HasType c) => Path (a -> b) -> Path (b -> c) -> Path (a -> c)
dot (Path p1) (Path p2) = Path (p1 ++ p2)

(<.>) :: (HasType a, HasType b, HasType c) => Path (a -> b) -> Path (b -> c) -> Path (a -> c)
(<.>) = dot

unsafePath :: (HasType a, HasType b) => [T.Text] -> Path (a -> b)
unsafePath = Path

get :: (HasType a, HasType b) => Path (a -> b) -> Expr a -> Expr b
get (Path path) expr = Expr $ Ast'Get $ Ast.Get path (toAst expr)

--

eq :: (HasType a) => Expr a -> Expr a -> Expr Bool
eq x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "==") [toAst x, toAst y])

neq :: (HasType a) => Expr a -> Expr a -> Expr Bool
neq x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "!=") [toAst x, toAst y])

add :: (Num a, HasType a) => Expr a -> Expr a -> Expr a
add x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "+") [toAst x, toAst y])

sub :: (Num a, HasType a) => Expr a -> Expr a -> Expr a
sub x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "-") [toAst x, toAst y])

mul :: (Num a, HasType a) => Expr a -> Expr a -> Expr a
mul x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "*") [toAst x, toAst y])

divide :: (Num a, HasType a) => Expr a -> Expr a -> Expr a
divide x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "/") [toAst x, toAst y])

concaT :: Expr T.Text -> Expr T.Text -> Expr T.Text
concaT x y = Expr (Ast'FnCall $ Ast.FnCall (Ast'Ref $ Ast.Ref "concat") [toAst x, toAst y])

--

class (HasType a, ToAst a) => ToExpr a where
  ex :: a -> Expr a

--

instance ToExpr () where
  ex _ = unit

instance ToExpr Bool where
  ex = bool

instance ToExpr T.Text where
  ex = string

instance ToExpr Int8 where
  ex = i8

instance ToExpr Int16 where
  ex = i16

instance ToExpr Int32 where
  ex = i32

instance ToExpr Int64 where
  ex = i64

instance ToExpr Word8 where
  ex = u8

instance ToExpr Word16 where
  ex = u16

instance ToExpr Word32 where
  ex = u32

instance ToExpr Word64 where
  ex = u64

instance ToExpr Float where
  ex = f32

instance ToExpr Double where
  ex = f64

--

unit :: Expr ()
unit = Expr (toAst ())

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

list :: (HasType a) => [Expr a] -> Expr [a]
list = Expr . Ast'List . Ast.List . map toAst

option :: (HasType a) => Maybe (Expr a) -> Expr (Maybe a)
option = \case
  Nothing -> Expr (Ast'Const Const'Null)
  Just expr -> Expr (toAst expr)

eitheR :: (HasType a, HasType b) => Either (Expr a) (Expr b) -> Expr (Either a b)
eitheR = \case
  Left expr -> Expr $ Ast'Enumeral $ Ast.Enumeral "Left" $ Just $ Map.fromList [("left", toAst expr)]
  Right expr -> Expr $ Ast'Enumeral $ Ast.Enumeral "Right" $ Just $ Map.fromList [("right", toAst expr)]

--

tuple2
  :: (HasType t1, HasType t2)
  => Expr t1 -> Expr t2
  -> Expr (t1, t2)
tuple2 t1 t2 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2]

tuple2'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2)
  => (t1, t2)
  -> Expr (t1, t2)
tuple2' t = Expr (toAst t)

tuple3
  :: (HasType t1, HasType t2, HasType t3)
  => Expr t1 -> Expr t2 -> Expr t3
  -> Expr (t1, t2, t3)
tuple3 t1 t2 t3 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3]

tuple3'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3)
  => (t1, t2, t3)
  -> Expr (t1, t2, t3)
tuple3' t = Expr (toAst t)

tuple4
  :: (HasType t1, HasType t2, HasType t3, HasType t4)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4
  -> Expr (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4]

tuple4'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4)
  => (t1, t2, t3, t4)
  -> Expr (t1, t2, t3, t4)
tuple4' t = Expr (toAst t)

tuple5
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5
  -> Expr (t1, t2, t3, t4, t5)
tuple5 t1 t2 t3 t4 t5 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5]

tuple5'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5)
  => (t1, t2, t3, t4, t5)
  -> Expr (t1, t2, t3, t4, t5)
tuple5' t = Expr (toAst t)

tuple6
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6
  -> Expr (t1, t2, t3, t4, t5, t6)
tuple6 t1 t2 t3 t4 t5 t6 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6]

tuple6'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6)
  => (t1, t2, t3, t4, t5, t6)
  -> Expr (t1, t2, t3, t4, t5, t6)
tuple6' t = Expr (toAst t)

tuple7
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7
  -> Expr (t1, t2, t3, t4, t5, t6, t7)
tuple7 t1 t2 t3 t4 t5 t6 t7 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7]

tuple7'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7)
  => (t1, t2, t3, t4, t5, t6, t7)
  -> Expr (t1, t2, t3, t4, t5, t6, t7)
tuple7' t = Expr (toAst t)

tuple8
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8)
tuple8 t1 t2 t3 t4 t5 t6 t7 t8 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8]

tuple8'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8)
  => (t1, t2, t3, t4, t5, t6, t7, t8)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8)
tuple8' t = Expr (toAst t)

tuple9
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9)
tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9]

tuple9'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9)
tuple9' t = Expr (toAst t)

tuple10
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10]

tuple10'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
tuple10' t = Expr (toAst t)

tuple11
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
tuple11 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11]

tuple11'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
tuple11' t = Expr (toAst t)

tuple12
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
tuple12 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12]

tuple12'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
tuple12' t = Expr (toAst t)

tuple13
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
tuple13 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13]

tuple13'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
tuple13' t = Expr (toAst t)

tuple14
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
tuple14 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14]

tuple14'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
tuple14' t = Expr (toAst t)

tuple15
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
tuple15 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15]

tuple15'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
tuple15' t = Expr (toAst t)

tuple16
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
tuple16 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16]

tuple16'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
tuple16' t = Expr (toAst t)

tuple17
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
tuple17 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17]

tuple17'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
tuple17' t = Expr (toAst t)

tuple18
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
tuple18 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18]

tuple18'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
tuple18' t = Expr (toAst t)

tuple19
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
tuple19 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19]

tuple19'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
tuple19' t = Expr (toAst t)

tuple20
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
tuple20 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20]

tuple20'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
tuple20' t = Expr (toAst t)

tuple21
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
tuple21 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21]

tuple21'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
tuple21' t = Expr (toAst t)

tuple22
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
tuple22 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22]

tuple22'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
tuple22' t = Expr (toAst t)

tuple23
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23)
tuple23 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23]

tuple23'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23)
tuple23' t = Expr (toAst t)

tuple24
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24)
tuple24 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24]

tuple24'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24)
tuple24' t = Expr (toAst t)

tuple25
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25)
tuple25 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25]

tuple25'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25)
tuple25' t = Expr (toAst t)

tuple26
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26)
tuple26 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26]

tuple26'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26)
tuple26' t = Expr (toAst t)

tuple27
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27)
tuple27 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27]

tuple27'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27)
tuple27' t = Expr (toAst t)

tuple28
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28)
tuple28 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28]

tuple28'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28)
tuple28' t = Expr (toAst t)

tuple29
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29)
tuple29 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29]

tuple29'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29)
tuple29' t = Expr (toAst t)

tuple30
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30)
tuple30 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30]

tuple30'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30)
tuple30' t = Expr (toAst t)

tuple31
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31)
tuple31 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31]

tuple31'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30, HasType t31, ToAst t31)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31)
tuple31' t = Expr (toAst t)

tuple32
  :: (HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31, HasType t32)
  => Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr t32
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32)
tuple32 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 = Expr $ Ast'Tuple $ Ast.Tuple [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31, toAst t32]

tuple32'
  :: (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30, HasType t31, ToAst t31, HasType t32, ToAst t32)
  => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32)
  -> Expr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32)
tuple32' t = Expr (toAst t)

instance (ToExpr t1, ToExpr t2) => ToExpr (t1, t2) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3) => ToExpr (t1, t2, t3) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4) => ToExpr (t1, t2, t3, t4) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5) => ToExpr (t1, t2, t3, t4, t5) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6) => ToExpr (t1, t2, t3, t4, t5, t6) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7) => ToExpr (t1, t2, t3, t4, t5, t6, t7) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27, ToExpr t28) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27, ToExpr t28, ToExpr t29) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27, ToExpr t28, ToExpr t29, ToExpr t30) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27, ToExpr t28, ToExpr t29, ToExpr t30, ToExpr t31) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
  ex = Expr . toAst

instance (ToExpr t1, ToExpr t2, ToExpr t3, ToExpr t4, ToExpr t5, ToExpr t6, ToExpr t7, ToExpr t8, ToExpr t9, ToExpr t10, ToExpr t11, ToExpr t12, ToExpr t13, ToExpr t14, ToExpr t15, ToExpr t16, ToExpr t17, ToExpr t18, ToExpr t19, ToExpr t20, ToExpr t21, ToExpr t22, ToExpr t23, ToExpr t24, ToExpr t25, ToExpr t26, ToExpr t27, ToExpr t28, ToExpr t29, ToExpr t30, ToExpr t31, ToExpr t32) => ToExpr (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
  ex = Expr . toAst

{-

var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var tuple = n => {
  if (n < 2) {
    return '';
  }

  var l = [
    'tuple', n, '\n',
    '  :: (HasType t1',
  ];
  for (var i = 1; i < n; i++) {
    l = l.concat([', HasType t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['  => Expr t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([' -> Expr t', i + 1]);
  }
  l = l.concat(['\n']);

  l = l.concat(['  -> Expr (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['tuple', n]);
  for (var i = 0; i < n; i++) {
    l = l.concat([' t', i + 1]);
  }
  l = l.concat([' = Expr $ Ast\'Tuple $ Ast.Tuple [toAst t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', toAst t', i + 1]);
  }
  l = l.concat([']\n\n']);

  return l.join('');
};

var tuplePure = n => {
  if (n < 2) {
    return '';
  }

  var l = [
    'tuple', n, '\'\n',
    '  :: (HasType t1, ToAst t1',
  ];
  for (var i = 1; i < n; i++) {
    l = l.concat([', HasType t', i + 1, ', ToAst t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['  => (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['  -> Expr (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([')\n']);

  l = l.concat(['tuple', n, '\' t = Expr (toAst t)\n\n']);

  return l.join('');
};

var tupleToExpr = n => {
  if (n < 2) {
    return '';
  }

  var l = [
    'instance (ToExpr t1',
  ];
  for (var i = 1; i < n; i++) {
    l = l.concat([', ToExpr t', i + 1]);
  }
  l = l.concat([') =>']);

  l = l.concat([' ToExpr (t1']);
  for (var i = 1; i < n; i++) {
    l = l.concat([', t', i + 1]);
  }
  l = l.concat([') where\n']);
  l = l.concat(['  ex = Expr . toAst\n\n']);
  return l.join('');
};

-}

--

class ToArgs a where
  toArgs :: a -> [Ast]

instance ToArgs () where
  toArgs _ = []

instance (ToAst a, HasType a) => ToArgs (Expr a) where
  toArgs x = [toAst x]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2) => ToArgs (t1, t2) where
  toArgs (t1, t2) = [toAst t1, toAst t2]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3) => ToArgs (t1, t2, t3) where
  toArgs (t1, t2, t3) = [toAst t1, toAst t2, toAst t3]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4) => ToArgs (t1, t2, t3, t4) where
  toArgs (t1, t2, t3, t4) = [toAst t1, toAst t2, toAst t3, toAst t4]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5) => ToArgs (t1, t2, t3, t4, t5) where
  toArgs (t1, t2, t3, t4, t5) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6) => ToArgs (t1, t2, t3, t4, t5, t6) where
  toArgs (t1, t2, t3, t4, t5, t6) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7) => ToArgs (t1, t2, t3, t4, t5, t6, t7) where
  toArgs (t1, t2, t3, t4, t5, t6, t7) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30, HasType t31, ToAst t31) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31]

instance (HasType t1, ToAst t1, HasType t2, ToAst t2, HasType t3, ToAst t3, HasType t4, ToAst t4, HasType t5, ToAst t5, HasType t6, ToAst t6, HasType t7, ToAst t7, HasType t8, ToAst t8, HasType t9, ToAst t9, HasType t10, ToAst t10, HasType t11, ToAst t11, HasType t12, ToAst t12, HasType t13, ToAst t13, HasType t14, ToAst t14, HasType t15, ToAst t15, HasType t16, ToAst t16, HasType t17, ToAst t17, HasType t18, ToAst t18, HasType t19, ToAst t19, HasType t20, ToAst t20, HasType t21, ToAst t21, HasType t22, ToAst t22, HasType t23, ToAst t23, HasType t24, ToAst t24, HasType t25, ToAst t25, HasType t26, ToAst t26, HasType t27, ToAst t27, HasType t28, ToAst t28, HasType t29, ToAst t29, HasType t30, ToAst t30, HasType t31, ToAst t31, HasType t32, ToAst t32) => ToArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
  toArgs (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) = [toAst t1, toAst t2, toAst t3, toAst t4, toAst t5, toAst t6, toAst t7, toAst t8, toAst t9, toAst t10, toAst t11, toAst t12, toAst t13, toAst t14, toAst t15, toAst t16, toAst t17, toAst t18, toAst t19, toAst t20, toAst t21, toAst t22, toAst t23, toAst t24, toAst t25, toAst t26, toAst t27, toAst t28, toAst t29, toAst t30, toAst t31, toAst t32]

{-
var tuples = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32];
var toArgs = n => {
  if (n < 2) {
    return '';
  }

  var l = ['instance (HasType t1, ToAst t1'];
  for (var i = 1; i < n; i++) {
    l = l.concat([', HasType t', i + 1, ', ToAst t', i + 1])
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

data Fn a

call :: ToArgs args => Expr (Fn (args -> a)) -> args -> Expr a
call (Expr f) args = Expr $ Ast'FnCall $ Ast.FnCall f (toArgs args)
call Expr'WrapCtor _ = error "Unevalated wrap contructor expression cannot be called"
call (Expr'MembersCtor _ _) _ = error "Unevalated member contructor expression cannot be called"

(-<) :: ToArgs args => Expr (Fn (args -> a)) -> args -> Expr a
f -< x = call f x

fn0
  :: (HasType a, ToAst a)
  => Expr a
  -> Expr (Fn (() -> a))
fn0 f = Expr $ Ast'Lambda $ Ast.Lambda
  []
  (toAst f)

fn1
  :: (HasType a, HasType t1)
  => Symbol
  -> (Expr t1 -> Expr a)
  -> Expr (Fn ((Expr t1) -> a))
fn1 s1 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f))]
  (toAst $ f (unsafeRef s1))
  where
    p1 :: (t1 -> a) -> Proxy t1
    p1 _ = Proxy

fn2
  :: (HasType a, HasType t1, HasType t2)
  => Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2) -> a))
fn2 s1 s2 f = Expr $ Ast'Lambda $ Ast.Lambda
  [(s1, getType (p1 f)), (s2, getType (p2 f))]
  (toAst $ f (unsafeRef s1) (unsafeRef s2))
  where
    p1 :: (t1 -> t2 -> a) -> Proxy t1
    p1 _ = Proxy
    p2 :: (t1 -> t2 -> a) -> Proxy t2
    p2 _ = Proxy

fn3
  :: (HasType a, HasType t1, HasType t2, HasType t3)
  => Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4)
  => Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31) -> a))
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
  :: (HasType a, HasType t1, HasType t2, HasType t3, HasType t4, HasType t5, HasType t6, HasType t7, HasType t8, HasType t9, HasType t10, HasType t11, HasType t12, HasType t13, HasType t14, HasType t15, HasType t16, HasType t17, HasType t18, HasType t19, HasType t20, HasType t21, HasType t22, HasType t23, HasType t24, HasType t25, HasType t26, HasType t27, HasType t28, HasType t29, HasType t30, HasType t31, HasType t32)
  => Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
  -> (Expr t1 -> Expr t2 -> Expr t3 -> Expr t4 -> Expr t5 -> Expr t6 -> Expr t7 -> Expr t8 -> Expr t9 -> Expr t10 -> Expr t11 -> Expr t12 -> Expr t13 -> Expr t14 -> Expr t15 -> Expr t16 -> Expr t17 -> Expr t18 -> Expr t19 -> Expr t20 -> Expr t21 -> Expr t22 -> Expr t23 -> Expr t24 -> Expr t25 -> Expr t26 -> Expr t27 -> Expr t28 -> Expr t29 -> Expr t30 -> Expr t31 -> Expr t32 -> Expr a)
  -> Expr (Fn ((Expr t1, Expr t2, Expr t3, Expr t4, Expr t5, Expr t6, Expr t7, Expr t8, Expr t9, Expr t10, Expr t11, Expr t12, Expr t13, Expr t14, Expr t15, Expr t16, Expr t17, Expr t18, Expr t19, Expr t20, Expr t21, Expr t22, Expr t23, Expr t24, Expr t25, Expr t26, Expr t27, Expr t28, Expr t29, Expr t30, Expr t31, Expr t32) -> a))
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

  l = l.concat(['  :: (HasType a, HasType t1']);
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

  l = l.concat(['  -> Expr (Fn ((Expr t1'])
  for (var i = 1; i < n; i++) {
    l = l.concat([', Expr t', i + 1]);
  }
  l = l.concat([') -> a))\n']);

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
