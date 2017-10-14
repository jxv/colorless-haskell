module Colorless.Server.Expr
  ( Expr(..)
  , EvalConfig(..)
  --
  , Ref(..)
  , UnVal(..)
  , UnEnumeral(..)
  , UnWrap(..)
  , UnStruct(..)
  , If(..)
  , Get(..)
  , Define(..)
  , Lambda(..)
  , Fn(..)
  , List(..)
  , Do(..)
  , FnCall(..)
  , ApiUnCall(..)
  , HollowUnCall(..)
  , WrapUnCall(..)
  , StructUnCall(..)
  , EnumerationUnCall(..)
  , Val(..)
  , ApiVal(..)
  , Wrap(..)
  , Struct(..)
  , Enumeral(..)
  , ApiCall(..)
  --
  , jsonToExpr
  , apiCallName
  , fromAst
  --
  , ApiParser(..)
  , parseApiCall
  --
  , eval
  , forceVal
  , runEval
  , emptyEnv
  ) where

import qualified Data.Map as Map
import Control.Monad (when, join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks, lift)
import Data.Map (Map)
import Data.Int
import Data.Word
import Data.Aeson (parseJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Data.Scientific (toBoundedInteger, toBoundedRealFloat, Scientific)
import Data.Text (Text)

import qualified Colorless.Ast as Ast
import Colorless.Types
import Colorless.Ast (Ast(..))
import Colorless.Val
import Colorless.Prim
import Colorless.RuntimeThrower

data EvalConfig m = EvalConfig
  { limits :: Limits
  , langServiceCallCount :: IORef Int
  , langLambdaCount :: IORef Int
  , langExprCount :: IORef Int
  , apiCall :: ApiCall -> m Val
  }

jsonToExpr :: (Monad m) => Value -> Maybe (Expr m)
jsonToExpr = fmap fromAst . parseMaybe parseJSON

newtype Eval m a = Eval (ReaderT (EvalConfig m) m a)
  deriving (Functor, Applicative, Monad, MonadReader (EvalConfig m), MonadIO)

instance RuntimeThrower m => RuntimeThrower (Eval m) where
  runtimeThrow err = Eval (lift $ runtimeThrow err)

tick :: (MonadIO m, RuntimeThrower m) => (Limits -> Maybe Int) -> (EvalConfig m -> IORef Int) -> (Int -> RuntimeError) -> Eval m ()
tick getLimit langCount err = do
  limit' <- getLimit <$> asks limits
  case limit' of
    Nothing -> return ()
    Just limit -> do
      ref <- asks langCount
      count <- liftIO $ readIORef ref
      if count == limit
        then runtimeThrow (err count)
        else liftIO $ writeIORef ref (count + 1)

tickServiceCall :: (MonadIO m, RuntimeThrower m) => Eval m ()
tickServiceCall = tick serviceCallLimit langServiceCallCount RuntimeError'LangServiceCallLimit

tickLambda :: (MonadIO m, RuntimeThrower m) => Eval m ()
tickLambda = tick lambdaLimit langLambdaCount RuntimeError'LangLambdaLimit

tickExpr :: (MonadIO m, RuntimeThrower m) => Eval m ()
tickExpr = tick exprLimit langExprCount RuntimeError'LangExprLimit

type Env m = Map Symbol (IORef (Expr m))

runEval :: MonadIO m => Eval m a -> EvalConfig m -> m a
runEval (Eval r) = runReaderT r

data Expr m
  = Expr'Ref Ref
  | Expr'UnVal (UnVal m)
  | Expr'Val Val
  | Expr'If (If m)
  | Expr'Get (Get m)
  | Expr'Define (Define m)
  | Expr'Lambda (Lambda m)
  | Expr'List (List m)
  | Expr'Tuple (Tuple m)
  | Expr'Fn (Fn m)
  | Expr'FnCall (FnCall m)
  | Expr'Do (Do m)
  | Expr'ApiUnCall (ApiUnCall m)
  deriving (Show, Eq)

data UnVal m
  = UnVal'Const Const
  | UnVal'UnWrap (UnWrap m)
  | UnVal'UnStruct (UnStruct m)
  | UnVal'UnEnumeral (UnEnumeral m)
  deriving (Show, Eq)

data UnWrap m = UnWrap
  { w :: Expr m
  } deriving (Show, Eq)

data UnStruct m = UnStruct
  { m :: Map MemberName (Expr m)
  } deriving (Show, Eq)

data UnEnumeral m = UnEnumeral
  { tag :: EnumeralName
  , m :: Maybe (Map MemberName (Expr m))
  } deriving (Show, Eq)

data Ref = Ref
  { symbol :: Symbol
  } deriving (Show, Eq)

data If m = If
  { cond :: Expr m
  , true :: Expr m
  , false :: Expr m
  } deriving (Show, Eq)

data Get m = Get
  { path :: [Text]
  , expr :: Expr m
  } deriving (Show, Eq)

data Define m = Define
  { var :: Symbol
  , expr :: Expr m
  } deriving (Show, Eq)

data Lambda m = Lambda
  { params :: [(Symbol, Type)]
  , expr :: Expr m
  } deriving (Show, Eq)

newtype Fn m = Fn ([Expr m] -> Eval m (Expr m))

instance Show (Fn m) where
  show _ = "<Fn>"

instance Eq (Fn m) where
  (==) _ _ = False

data List m = List
  { list :: [Expr m]
  } deriving (Show, Eq)

data Tuple m = Tuple
  { tuple :: [Expr m]
  } deriving (Show, Eq)

data Do m = Do
  { exprs :: [Expr m]
  } deriving (Show, Eq)

data FnCall m = FnCall
  { fn :: Expr m
  , args :: [Expr m]
  } deriving (Show, Eq)

data ApiUnCall m
  = ApiUnCall'HollowUnCall HollowUnCall
  | ApiUnCall'WrapUnCall (WrapUnCall m)
  | ApiUnCall'StructUnCall (StructUnCall m)
  | ApiUnCall'EnumerationUnCall (EnumerationUnCall m)
  deriving (Show, Eq)

data HollowUnCall = HollowUnCall
  { n :: TypeName
  } deriving (Show, Eq)

data WrapUnCall m = WrapUnCall
  { n :: TypeName
  , w :: Expr m
  } deriving (Show, Eq)

data StructUnCall m = StructUnCall
  { n :: TypeName
  , m :: Expr m
  } deriving (Show, Eq)

data EnumerationUnCall m = EnumerationUnCall
  { n :: TypeName
  , e :: Expr m
  } deriving (Show, Eq)

data ApiCall
  = ApiCall'Hollow TypeName
  | ApiCall'Struct TypeName Struct
  | ApiCall'Enumeration TypeName Enumeral
  | ApiCall'Wrap TypeName Wrap
  deriving (Show, Eq)

apiCallName :: ApiCall -> TypeName
apiCallName = \case
  ApiCall'Hollow n -> n
  ApiCall'Struct n _ -> n
  ApiCall'Enumeration n _ -> n
  ApiCall'Wrap n _ -> n

--

fromAst :: Monad m => Ast -> Expr m
fromAst = \case
  Ast'Ref Ast.Ref{symbol} -> Expr'Ref $ Ref symbol
  Ast'If Ast.If{cond,true,false} -> Expr'If $ If (fromAst cond) (fromAst true) (fromAst false)
  Ast'Get Ast.Get{path,val} -> Expr'Get $ Get path (fromAst val)
  Ast'Define Ast.Define{var,expr} -> Expr'Define $ Define var (fromAst expr)
  Ast'Lambda Ast.Lambda{args,expr} -> Expr'Lambda $ Lambda args (fromAst expr)
  Ast'List Ast.List{list} -> Expr'List $ List $ map fromAst list
  Ast'Tuple Ast.Tuple{tuple} -> Expr'Tuple $ Tuple $ map fromAst tuple
  Ast'Do Ast.Do{vals} -> Expr'Do $ Do $ map fromAst vals
  Ast'FnCall Ast.FnCall{fn,args} -> Expr'FnCall $ FnCall (fromAst fn) (map fromAst args)
  Ast'WrapCall Ast.WrapCall{n,w} -> Expr'ApiUnCall $ ApiUnCall'WrapUnCall $ WrapUnCall n (fromAst w)
  Ast'HollowCall Ast.HollowCall{n} -> Expr'ApiUnCall $ ApiUnCall'HollowUnCall $ HollowUnCall n
  Ast'StructCall Ast.StructCall{n,m} -> Expr'ApiUnCall $ ApiUnCall'StructUnCall $ StructUnCall n (fromAst m)
  Ast'EnumerationCall Ast.EnumerationCall{n,e} -> Expr'ApiUnCall $ ApiUnCall'EnumerationUnCall $ EnumerationUnCall n (fromAst e)
  Ast'Enumeral Ast.Enumeral{tag,m} -> Expr'UnVal $ UnVal'UnEnumeral $ UnEnumeral tag (fmap fromAst <$> m)
  Ast'Struct Ast.Struct{m} -> Expr'UnVal $ UnVal'UnStruct $ UnStruct (fromAst <$> m)
  Ast'Wrap Ast.Wrap{w} -> Expr'UnVal $ UnVal'UnWrap $ UnWrap (fromAst w)
  Ast'Const c -> Expr'UnVal $ UnVal'Const c

--

addEnvToEnv :: (RuntimeThrower m, Ord k, MonadIO m) => Maybe Int -> Map k a -> IORef (Map k a) -> m (IORef (Map k a))
addEnvToEnv maybeVariableLimit vars envRef = do
  env <- liftIO $ readIORef envRef
  let env' = Map.union vars env
  case maybeVariableLimit of
    Nothing -> return ()
    Just limit -> when (Map.size env' > limit) $ runtimeThrow RuntimeError'VariableLimit
  liftIO $ newIORef env'

addVarToEnv :: (Ord k, MonadIO m, RuntimeThrower m) => Maybe Int -> IORef (Map k a) -> k -> a -> Map k a -> m ()
addVarToEnv maybeVariableLimit envRef var ref env = do
  let env' = Map.insert var ref env
  case maybeVariableLimit of
    Nothing -> return ()
    Just limit -> when (Map.size env' > limit) $ runtimeThrow RuntimeError'VariableLimit
  liftIO $ writeIORef envRef env'

varLookup :: (MonadIO m, RuntimeThrower m) => Map Symbol (IORef a) -> Symbol -> m a
varLookup env symbol@(Symbol s) = case Map.lookup symbol env of
  Nothing -> runtimeThrow $ RuntimeError'UnknownVariable s
  Just var -> liftIO $ readIORef $ var

--

eval :: (MonadIO m, RuntimeThrower m) => Expr m -> IORef (Env m) -> Eval m (Expr m)
eval expr envRef = case expr of
  Expr'Ref atom -> evalRef atom envRef
  Expr'If if' -> evalIf if' envRef
  Expr'UnVal unVal -> evalUnVal unVal envRef
  Expr'Val val -> return $ Expr'Val val
  Expr'Get get -> evalGet get envRef
  Expr'Define define -> evalDefine define envRef
  Expr'Lambda lambda -> evalLambda lambda envRef
  Expr'Fn _ -> return expr -- throw error?
  Expr'List list -> evalList list envRef
  Expr'Tuple tuple -> evalTuple tuple envRef
  Expr'FnCall call -> evalFnCall call envRef
  Expr'Do dO -> evalDo dO envRef
  Expr'ApiUnCall apiUnCall -> evalApiUnCall apiUnCall envRef

forceVal :: (RuntimeThrower m) => Expr m -> Eval m Val
forceVal (Expr'Val v) = return v
forceVal (Expr'List (List l)) = Val'List <$> mapM forceVal l
forceVal (Expr'Tuple (Tuple t)) = Val'List <$> mapM forceVal t
forceVal _ = runtimeThrow RuntimeError'IncompatibleType

evalRef :: (MonadIO m, RuntimeThrower m) => Ref -> IORef (Env m) -> Eval m (Expr m)
evalRef Ref{symbol} envRef = do
  tickExpr
  env <- liftIO $ readIORef envRef
  varLookup env symbol

evalUnVal :: (MonadIO m, RuntimeThrower m) => UnVal m -> IORef (Env m) -> Eval m (Expr m)
evalUnVal unVal envRef = case unVal of
  UnVal'Const c -> return $ Expr'Val $ Val'Const c

  UnVal'UnStruct UnStruct{m} -> do
    members <- mapM (\(name,expr) -> (name,) <$> (forceVal =<< eval expr envRef)) (Map.toList m)
    return $ Expr'Val $ Val'ApiVal $ ApiVal'Struct $ Struct (Map.fromList members)

  UnVal'UnWrap UnWrap{w} -> do
    w' <- eval w envRef
    case w' of
      Expr'Val (Val'Const c) -> return $ Expr'Val $ Val'Const c
      _ -> runtimeThrow RuntimeError'IncompatibleType

  UnVal'UnEnumeral UnEnumeral{tag,m} -> do
    case m of
      Nothing -> return $ Expr'Val $ Val'ApiVal $ ApiVal'Enumeral $ Enumeral tag Nothing
      Just members' -> do
        members <- mapM (\(name,expr) -> (name,) <$> (forceVal =<< eval expr envRef)) (Map.toList members')
        return $ Expr'Val $ Val'ApiVal $ ApiVal'Enumeral $ Enumeral tag (Just $ Map.fromList members)

evalIf :: (MonadIO m, RuntimeThrower m) => If m -> IORef (Env m) -> Eval m (Expr m)
evalIf If{cond, true, false} envRef = do
  tickExpr
  envRef' <- liftIO $ newIORef =<< readIORef envRef
  v <- eval cond envRef'
  case v of
    Expr'Val (Val'Const (Const'Bool cond')) -> do
      envRef'' <- liftIO $ newIORef =<< readIORef envRef
      eval (if cond' then true else false) envRef''
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalGet :: (MonadIO m, RuntimeThrower m) => Get m -> IORef (Env m) -> Eval m (Expr m)
evalGet Get{path,expr} envRef = do
  tickExpr
  getter path =<< eval expr envRef

getter :: (MonadIO m, RuntimeThrower m) => [Text] -> Expr m -> Eval m (Expr m)
getter [] expr = return expr
getter path expr =
  case expr of
    Expr'Val val -> case val of
      Val'ApiVal apiVal -> getterApiVal path apiVal
      _ -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'IncompatibleType

getterApiVal :: (MonadIO m, RuntimeThrower m) => [Text] -> ApiVal -> Eval m (Expr m)
getterApiVal (mName:path) (ApiVal'Struct Struct{m}) =
  case Map.lookup (MemberName mName) m of
    Nothing -> runtimeThrow RuntimeError'IncompatibleType
    Just member -> getter path (Expr'Val member)
getterApiVal (mName:path) (ApiVal'Enumeral Enumeral{m})
  | mName == "tag" = runtimeThrow RuntimeError'IncompatibleType
  | otherwise = case m >>= Map.lookup (MemberName mName) of
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
      Just member -> getter path (Expr'Val member)
getterApiVal _ _ = runtimeThrow RuntimeError'IncompatibleType

evalDefine :: (MonadIO m, RuntimeThrower m) => Define m -> IORef (Env m) -> Eval m (Expr m)
evalDefine Define{var, expr} envRef = do
  tickExpr
  expr' <- eval expr envRef
  env <- liftIO $ readIORef envRef
  ref <- liftIO $ newIORef expr'
  limit <- variableLimit <$> asks limits
  addVarToEnv limit envRef var ref env
  return expr'

evalLambda :: (MonadIO m, RuntimeThrower m) => Lambda m -> IORef (Env m) -> Eval m (Expr m)
evalLambda Lambda{params, expr} envRef = do
  tickLambda
  tickExpr
  return . Expr'Fn . Fn $ \vals -> do
    let keys = map fst params
    let args = zip keys vals
    let keysLen = length keys
    let argsLen = length args
    if keysLen /= argsLen
      then runtimeThrow $ if keysLen < argsLen
        then RuntimeError'TooManyArguments
        else RuntimeError'TooFewArguments
      else do
        args' <- liftIO $ mapM newIORef (Map.fromList args)
        limit <- variableLimit <$> asks limits
        envRef' <- addEnvToEnv limit args' envRef
        eval expr envRef'

evalList :: (MonadIO m, RuntimeThrower m) => List m -> IORef (Env m)-> Eval m (Expr m)
evalList List{list} envRef = do
  tickExpr
  list' <- mapM (\item -> eval item envRef) list
  return . Expr'List $ List list'

evalTuple :: (MonadIO m, RuntimeThrower m) => Tuple m -> IORef (Env m)-> Eval m (Expr m)
evalTuple Tuple{tuple} envRef = do
  tickExpr
  tuple' <- mapM (\item -> eval item envRef) tuple
  return . Expr'Tuple $ Tuple tuple'

evalDo :: (MonadIO m, RuntimeThrower m) => Do m -> IORef (Env m) -> Eval m (Expr m)
evalDo Do{exprs} envRef = do
  tickExpr
  case exprs of
    [] -> return $ Expr'Val $ Val'Const $ Const'Null
    _ -> last <$> mapM (\expr -> eval expr envRef) exprs

evalFnCall :: (MonadIO m, RuntimeThrower m) => FnCall m -> IORef (Env m) -> Eval m (Expr m)
evalFnCall FnCall{fn, args} envRef = do
  tickExpr
  val <- eval fn envRef
  case val of
    Expr'Fn (Fn fn') -> do
      args' <- mapM (\arg -> eval arg envRef) args
      fn' args'
    Expr'Ref Ref{symbol} -> do
      env <- liftIO $ readIORef envRef
      v <- varLookup env symbol
      case v of
        Expr'Fn (Fn fn') -> do
          args' <- mapM (\arg -> eval arg envRef) args
          fn' args'
        _ -> runtimeThrow $ RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalApiUnCall :: (MonadIO m, RuntimeThrower m) => ApiUnCall m -> IORef (Env m) -> Eval m (Expr m)
evalApiUnCall apiUnCall envRef = do
  tickServiceCall
  tickExpr
  Expr'Val <$> case apiUnCall of
    ApiUnCall'HollowUnCall c -> evalHollowUnCall c
    ApiUnCall'WrapUnCall c -> evalWrapUnCall c envRef
    ApiUnCall'StructUnCall c -> evalStructUnCall c envRef
    ApiUnCall'EnumerationUnCall c -> evalEnumerationUnCall c envRef

evalHollowUnCall :: (MonadIO m, RuntimeThrower m) => HollowUnCall -> Eval m Val
evalHollowUnCall HollowUnCall{n} =
  Eval . ReaderT $ \cfg ->
    apiCall cfg $ ApiCall'Hollow n

evalWrapUnCall :: (MonadIO m, RuntimeThrower m) => WrapUnCall m -> IORef (Env m) -> Eval m Val
evalWrapUnCall WrapUnCall{n,w} envRef = do
  expr <- eval w envRef
  case expr of
    Expr'Val (Val'Const c) -> Eval . ReaderT $ \cfg ->
      apiCall cfg $ ApiCall'Wrap n (Wrap c)
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalStructUnCall :: (MonadIO m, RuntimeThrower m) => StructUnCall m -> IORef (Env m) -> Eval m Val
evalStructUnCall StructUnCall{n,m} envRef = do
  expr <- eval m envRef
  case expr of
    Expr'Val (Val'ApiVal (ApiVal'Struct m')) -> Eval . ReaderT $ \cfg ->
      apiCall cfg $ ApiCall'Struct n m'
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalEnumerationUnCall :: (MonadIO m, RuntimeThrower m) => EnumerationUnCall m -> IORef (Env m) -> Eval m Val
evalEnumerationUnCall EnumerationUnCall{n,e} envRef = do
  expr <- eval e envRef
  case expr of
    Expr'Val (Val'ApiVal (ApiVal'Enumeral e')) -> Eval . ReaderT $ \cfg ->
      apiCall cfg $ ApiCall'Enumeration n e'
    _ -> runtimeThrow RuntimeError'IncompatibleType

emptyEnv :: RuntimeThrower m => IO (IORef (Env m))
emptyEnv = do
  eq <- newIORef eqExpr
  neq <- newIORef neqExpr
  concat' <- newIORef concatExpr

  addI8 <- newIORef $ i8Expr (+)
  addI16 <- newIORef $ i16Expr (+)
  addI32 <- newIORef $ i32Expr (+)
  addI64 <- newIORef $ i64Expr (+)
  addU8 <- newIORef $ u8Expr (+)
  addU16 <- newIORef $ u16Expr (+)
  addU32 <- newIORef $ u32Expr (+)
  addU64 <- newIORef $ u64Expr (+)
  addF32 <- newIORef $ f32Expr (+)
  addF64 <- newIORef $ f64Expr (+)

  subI8 <- newIORef $ i8Expr (-)
  subI16 <- newIORef $ i16Expr (-)
  subI32 <- newIORef $ i32Expr (-)
  subI64 <- newIORef $ i64Expr (-)
  subU8 <- newIORef $ u8Expr (-)
  subU16 <- newIORef $ u16Expr (-)
  subU32 <- newIORef $ u32Expr (-)
  subU64 <- newIORef $ u64Expr (-)
  subF32 <- newIORef $ f32Expr (-)
  subF64 <- newIORef $ f64Expr (-)

  mulI8 <- newIORef $ i8Expr (*)
  mulI16 <- newIORef $ i16Expr (*)
  mulI32 <- newIORef $ i32Expr (*)
  mulI64 <- newIORef $ i64Expr (*)
  mulU8 <- newIORef $ u8Expr (*)
  mulU16 <- newIORef $ u16Expr (*)
  mulU32 <- newIORef $ u32Expr (*)
  mulU64 <- newIORef $ u64Expr (*)
  mulF32 <- newIORef $ f32Expr (*)
  mulF64 <- newIORef $ f64Expr (*)

  divI8 <- newIORef $ i8Expr (div)
  divI16 <- newIORef $ i16Expr (div)
  divI32 <- newIORef $ i32Expr (div)
  divI64 <- newIORef $ i64Expr (div)
  divU8 <- newIORef $ u8Expr (div)
  divU16 <- newIORef $ u16Expr (div)
  divU32 <- newIORef $ u32Expr (div)
  divU64 <- newIORef $ u64Expr (div)
  divF32 <- newIORef $ f32Expr (/)
  divF64 <- newIORef $ f64Expr (/)

  tuple <- newIORef tupleExpr

  mapList <- newIORef mapListExpr

  newIORef $ Map.fromList
    [ ("eq", eq)
    , ("neq", neq)

    , ("addI8", addI8)
    , ("addI16", addI16)
    , ("addI32", addI32)
    , ("addI64", addI64)
    , ("addU8", addU8)
    , ("addU16", addU16)
    , ("addU32", addU32)
    , ("addU64", addU64)
    , ("addF32", addF32)
    , ("addF64", addF64)

    , ("subI8", subI8)
    , ("subI16", subI16)
    , ("subI32", subI32)
    , ("subI64", subI64)
    , ("subU8", subU8)
    , ("subU16", subU16)
    , ("subU32", subU32)
    , ("subU64", subU64)
    , ("subF32", subF32)
    , ("subF64", subF64)

    , ("mulI8", mulI8)
    , ("mulI16", mulI16)
    , ("mulI32", mulI32)
    , ("mulI64", mulI64)
    , ("mulU8", mulU8)
    , ("mulU16", mulU16)
    , ("mulU32", mulU32)
    , ("mulU64", mulU64)
    , ("mulF32", mulF32)
    , ("mulF64", mulF64)

    , ("divI8", divI8)
    , ("divI16", divI16)
    , ("divI32", divI32)
    , ("divI64", divI64)
    , ("divU8", divU8)
    , ("divU16", divU16)
    , ("divU32", divU32)
    , ("divU64", divU64)
    , ("divF32", divF32)
    , ("divF64", divF64)

    , ("concat", concat')
    , ("tuple", tuple)

    , ("mapList", mapList)

    ]

mapListExpr :: RuntimeThrower m => Expr m
mapListExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), Expr'List (List list)] -> Expr'List . List <$> mapM (f . (:[])) list
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments

i8Expr :: RuntimeThrower m => (Int8 -> Int8 -> Int8) -> Expr m
i8Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'I8 x)), Expr'Val (Val'Prim (Prim'I8 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I8 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I8 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

i16Expr :: RuntimeThrower m => (Int16 -> Int16 -> Int16) -> Expr m
i16Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'I16 x)), Expr'Val (Val'Prim (Prim'I16 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I16 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I16 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

i32Expr :: RuntimeThrower m => (Int32 -> Int32 -> Int32) -> Expr m
i32Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'I32 x)), Expr'Val (Val'Prim (Prim'I32 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I32 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I32 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

i64Expr :: RuntimeThrower m => (Int64 -> Int64 -> Int64) -> Expr m
i64Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'I64 x)), Expr'Val (Val'Prim (Prim'I64 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I64 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I64 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

u8Expr :: RuntimeThrower m => (Word8 -> Word8 -> Word8) -> Expr m
u8Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'U8 x)), Expr'Val (Val'Prim (Prim'U8 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U8 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U8 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

u16Expr :: RuntimeThrower m => (Word16 -> Word16 -> Word16) -> Expr m
u16Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'U16 x)), Expr'Val (Val'Prim (Prim'U16 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U16 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U16 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

u32Expr :: RuntimeThrower m => (Word32 -> Word32 -> Word32) -> Expr m
u32Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'U32 x)), Expr'Val (Val'Prim (Prim'U32 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U32 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U32 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

u64Expr :: RuntimeThrower m => (Word64 -> Word64 -> Word64) -> Expr m
u64Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'U64 x)), Expr'Val (Val'Prim (Prim'U64 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U64 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U64 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

f32Expr :: RuntimeThrower m => (Float -> Float -> Float) -> Expr m
f32Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'F32 x)), Expr'Val (Val'Prim (Prim'F32 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'F32 y))] -> case toBoundedRealFloat x of
      x' -> toExpr $ eitherCollapse x' `op` y
    [Expr'Val (Val'Prim (Prim'F32 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedRealFloat y of
      y' -> toExpr $ x `op` eitherCollapse y'
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedRealFloat x, toBoundedRealFloat y) of
      (x', y') -> toExpr $ eitherCollapse x' `op` eitherCollapse y'
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

f64Expr :: RuntimeThrower m => (Double -> Double -> Double) -> Expr m
f64Expr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'F64 x)), Expr'Val (Val'Prim (Prim'F64 y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'F64 y))] -> case toBoundedRealFloat x of
      x' -> toExpr $ eitherCollapse x' `op` y
    [Expr'Val (Val'Prim (Prim'F64 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedRealFloat y of
      y' -> toExpr $ x `op` eitherCollapse y'
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> case (toBoundedRealFloat x, toBoundedRealFloat y) of
      (x', y') -> toExpr $ eitherCollapse x' `op` eitherCollapse y'
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

boolExpr :: RuntimeThrower m
  => (Scientific -> Scientific -> Bool)
  -> (Int8 -> Int8 -> Bool)
  -> (Int16 -> Int16 -> Bool)
  -> (Int32 -> Int32 -> Bool)
  -> (Int64 -> Int64 -> Bool)
  -> (Word8 -> Word8 -> Bool)
  -> (Word16 -> Word16 -> Bool)
  -> (Word32 -> Word32 -> Bool)
  -> (Word64 -> Word64 -> Bool)
  -> (Val -> Val -> Bool)
  -> Expr m
boolExpr num i8 i16 i32 i64 u8 u16 u32 u64 val = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooManyArguments
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Const (Const'Number y))] -> toExpr $ x `num` y

    [Expr'Val (Val'Prim (Prim'I8 x)), Expr'Val (Val'Prim (Prim'I8 y))] -> toExpr $ x `i8` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I8 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `i8` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I8 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `i8` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'I16 x)), Expr'Val (Val'Prim (Prim'I16 y))] -> toExpr $ x `i16` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I16 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `i16` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I16 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `i16` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'I32 x)), Expr'Val (Val'Prim (Prim'I32 y))] -> toExpr $ x `i32` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I32 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `i32` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I32 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `i32` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'I64 x)), Expr'Val (Val'Prim (Prim'I64 y))] -> toExpr $ x `i64` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'I64 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `i64` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'I64 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `i64` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'U8 x)), Expr'Val (Val'Prim (Prim'U8 y))] -> toExpr $ x `u8` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U8 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `u8` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U8 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `u8` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'U16 x)), Expr'Val (Val'Prim (Prim'U16 y))] -> toExpr $ x `u16` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U16 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `u16` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U16 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `u16` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'U32 x)), Expr'Val (Val'Prim (Prim'U32 y))] -> toExpr $ x `u32` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U32 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `u32` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U32 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `u32` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'U64 x)), Expr'Val (Val'Prim (Prim'U64 y))] -> toExpr $ x `u64` y
    [Expr'Val (Val'Const (Const'Number x)), Expr'Val (Val'Prim (Prim'U64 y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `u64` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'U64 x)), Expr'Val (Val'Const (Const'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `u64` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val x, Expr'Val y] -> toExpr $ x `val` y

    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

eqExpr :: RuntimeThrower m => Expr m
eqExpr = boolExpr (==) (==) (==) (==) (==) (==) (==) (==) (==) (==)

neqExpr :: RuntimeThrower m => Expr m
neqExpr = boolExpr (/=) (/=) (/=) (/=) (/=) (/=) (/=) (/=) (/=) (/=)

concatExpr :: RuntimeThrower m => Expr m
concatExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [x, y] -> case (x,y) of
      (Expr'Val (Val'Const (Const'String x')), Expr'Val (Val'Const (Const'String y'))) -> return $
        Expr'Val . Val'Const . Const'String $ x' `mappend` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments

tupleExpr :: RuntimeThrower m => Expr m
tupleExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    xs -> return $ Expr'Tuple $ Tuple xs

eitherCollapse :: Either a a -> a
eitherCollapse = either id id

--

data ApiParser api = ApiParser
  { hollow :: Map TypeName api
  , struct :: Map TypeName (Val -> Maybe api)
  , enumeration :: Map TypeName (Val -> Maybe api)
  , wrap :: Map TypeName (Val -> Maybe api)
  }

parseApiCall :: ApiParser api -> ApiCall -> Maybe api
parseApiCall ApiParser{hollow, struct, enumeration, wrap} = \case
  ApiCall'Hollow n -> Map.lookup n hollow
  ApiCall'Struct n s -> join $ ($ Val'ApiVal (ApiVal'Struct s)) <$> Map.lookup n struct
  ApiCall'Enumeration n e -> join $ ($ Val'ApiVal (ApiVal'Enumeral e)) <$> Map.lookup n enumeration
  ApiCall'Wrap n (Wrap w) -> join $ ($ Val'Const w) <$> Map.lookup n wrap
