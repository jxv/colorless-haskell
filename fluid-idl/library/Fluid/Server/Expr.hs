module Fluid.Server.Expr
  ( Expr(..)
  , EvalConfig(..)
  --
  , Ref(..)
  , UnVal(..)
  , UnEnumeral(..)
  , UnWrap(..)
  , UnStruct(..)
  , If(..)
  , Iflet(..)
  , Get(..)
  , Define(..)
  , Match(..)
  , MatchCase(..)
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
import Control.Monad (when, join, filterM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks, lift)
import Data.Map (Map)
import Data.Foldable (foldlM)
import Data.Aeson (parseJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Data.Scientific (toBoundedInteger, toRealFloat, Scientific)
import Data.Text (Text)

import qualified Fluid.Ast as Ast
import Fluid.Types
import Fluid.Ast (Ast(..))
import Fluid.Val
import Fluid.Prim
import Fluid.RuntimeThrower

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
tickServiceCall = tick serviceCalls langServiceCallCount RuntimeError'LangServiceCallLimit

tickLambda :: (MonadIO m, RuntimeThrower m) => Eval m ()
tickLambda = tick lambdas langLambdaCount RuntimeError'LangLambdaLimit

tickExpr :: (MonadIO m, RuntimeThrower m) => Eval m ()
tickExpr = tick expressions langExprCount RuntimeError'LangExprLimit

type Env m = Map Symbol (IORef (Expr m))

runEval :: MonadIO m => Eval m a -> EvalConfig m -> m a
runEval (Eval r) = runReaderT r

data Expr m
  = Expr'Ref Ref
  | Expr'UnVal (UnVal m)
  | Expr'Val Val
  | Expr'If (If m)
  | Expr'Iflet (Iflet m)
  | Expr'Get (Get m)
  | Expr'Set (Set m)
  | Expr'Match (Match m)
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

data Iflet m = Iflet
  { symbol :: Symbol
  , option :: Expr m
  , some :: Expr m
  , none :: Expr m
  } deriving (Show, Eq)

data Get m = Get
  { path :: [Text]
  , expr :: Expr m
  } deriving (Show, Eq)

data Set m = Set
  { path :: [Text]
  , src :: Expr m
  , dest :: Expr m
  } deriving (Show, Eq)

data MatchCase m
  = MatchCase'Tag (Expr m)
  | MatchCase'Members Symbol (Expr m)
  deriving (Show, Eq)

data Match m = Match
  { enumeral :: Expr m
  , cases :: Map EnumeralName (MatchCase m)
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
  Ast'Iflet Ast.Iflet{symbol, option, some, none} -> Expr'Iflet $ Iflet symbol (fromAst option) (fromAst some) (fromAst none)
  Ast'Get Ast.Get{path,val} -> Expr'Get $ Get path (fromAst val)
  Ast'Set Ast.Set{path,src,dest} -> Expr'Set $ Set path (fromAst src) (fromAst dest)
  Ast'Define Ast.Define{var,expr} -> Expr'Define $ Define var (fromAst expr)
  Ast'Match Ast.Match{enumeral,cases} -> Expr'Match $ Match (fromAst enumeral) (fromAstMatchCases cases)
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

fromAstMatchCases :: Monad m => [Ast.MatchCase] -> Map EnumeralName (MatchCase m)
fromAstMatchCases = Map.fromList . map cvt
  where
    cvt (Ast.MatchCase'Tag name ast) = (name, MatchCase'Tag (fromAst ast))
    cvt (Ast.MatchCase'Members name sym ast) = (name, MatchCase'Members sym (fromAst ast))

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

addVarToScope :: (MonadIO m, RuntimeThrower m) => IORef (Env m) -> Symbol -> Expr m -> Eval m ()
addVarToScope envRef var expr = do
  env <- liftIO $ readIORef envRef
  ref <- liftIO $ newIORef expr
  limit <- variables <$> asks limits
  addVarToEnv limit envRef var ref env

varLookup :: (MonadIO m, RuntimeThrower m) => Map Symbol (IORef a) -> Symbol -> m a
varLookup env symbol@(Symbol s) = case Map.lookup symbol env of
  Nothing -> runtimeThrow $ RuntimeError'UnknownVariable s
  Just var -> liftIO $ readIORef $ var

--

eval :: (MonadIO m, RuntimeThrower m) => Expr m -> IORef (Env m) -> Eval m (Expr m)
eval expr envRef = case expr of
  Expr'Ref atom -> evalRef atom envRef
  Expr'If if' -> evalIf if' envRef
  Expr'Iflet iflet -> evalIflet iflet envRef
  Expr'UnVal unVal -> evalUnVal unVal envRef
  Expr'Val val -> return $ Expr'Val val
  Expr'Get get -> evalGet get envRef
  Expr'Set set -> evalSet set envRef
  Expr'Define define -> evalDefine define envRef
  Expr'Match match -> evalMatch match envRef
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
  UnVal'Const c -> return $ Expr'Val $ case c of
    Const'Null -> Val'Infer Infer'Null
    Const'Number n -> Val'Infer (Infer'Number n)
    Const'Bool b -> Val'Prim (Prim'Bool b)
    Const'String s -> Val'Prim (Prim'String s)

  UnVal'UnStruct UnStruct{m} -> do
    members <- mapM (\(name,expr) -> (name,) <$> (forceVal =<< eval expr envRef)) (Map.toList m)
    return $ Expr'Val $ Val'ApiVal $ ApiVal'Struct $ Struct (Map.fromList members)

  UnVal'UnWrap UnWrap{w} -> do
    w' <- eval w envRef
    case w' of
      Expr'Val (Val'Infer c) -> return $ Expr'Val $ Val'Infer c
      Expr'Val (Val'Prim p) -> return $ Expr'Val $ Val'Prim p
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
    Expr'Val (Val'Prim (Prim'Bool cond')) -> do
      envRef'' <- liftIO $ newIORef =<< readIORef envRef
      eval (if cond' then true else false) envRef''
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalIflet :: (MonadIO m, RuntimeThrower m) => Iflet m -> IORef (Env m) -> Eval m (Expr m)
evalIflet Iflet{symbol, option, some, none} envRef = do
  tickExpr
  envRef' <- liftIO $ newIORef =<< readIORef envRef
  option' <- eval option envRef'
  case option' of
    Expr'Val (Val'Infer Infer'Null) -> eval none envRef'
    some' -> do
      envRef'' <- liftIO $ newIORef =<< readIORef envRef
      addVarToScope envRef'' symbol some'
      eval some envRef''

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

evalSet :: (MonadIO m, RuntimeThrower m) => Set m -> IORef (Env m) -> Eval m (Expr m)
evalSet Set{path,src,dest} envRef = do
  tickExpr
  dest' <- eval dest envRef
  src' <- eval src envRef
  setter path src' dest'

setter :: (MonadIO m, RuntimeThrower m) => [Text] -> Expr m -> Expr m -> Eval m (Expr m)
setter [] src _ = return src
setter path src dest =
  case dest of
    Expr'Val destVal -> case destVal of
      Val'ApiVal destApiVal -> setterApiVal path src destApiVal
      _ -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'IncompatibleType

setterApiVal :: (MonadIO m, RuntimeThrower m) => [Text] -> Expr m -> ApiVal -> Eval m (Expr m)
setterApiVal (mName:path) src (ApiVal'Struct Struct{m}) =
  case Map.lookup (MemberName mName) m of
    Nothing -> runtimeThrow RuntimeError'IncompatibleType
    Just member -> do
      exprMember' <- setter path src (Expr'Val member)
      case exprMember' of
        Expr'Val member' -> return . Expr'Val . Val'ApiVal . ApiVal'Struct . Struct $
          Map.insert (MemberName mName) member' m
        _ -> runtimeThrow RuntimeError'IncompatibleType -- Needs a Val
setterApiVal (mName:path) src (ApiVal'Enumeral Enumeral{tag, m})
  | mName == "tag" = runtimeThrow RuntimeError'IncompatibleType
  | otherwise = case m of
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
      Just members -> case Map.lookup (MemberName mName) members of
        Nothing -> runtimeThrow RuntimeError'IncompatibleType
        Just member -> do
          exprMember' <- setter path src (Expr'Val member)
          case exprMember' of
            Expr'Val member' -> return . Expr'Val . Val'ApiVal . ApiVal'Enumeral $
              Enumeral { tag = tag, m = Just $ Map.insert (MemberName mName) member' members }
            _ -> runtimeThrow RuntimeError'IncompatibleType -- Needs a Val
setterApiVal _ _ _ = runtimeThrow RuntimeError'IncompatibleType

evalDefine :: (MonadIO m, RuntimeThrower m) => Define m -> IORef (Env m) -> Eval m (Expr m)
evalDefine Define{var, expr} envRef = do
  tickExpr
  expr' <- eval expr envRef
  addVarToScope envRef var expr'
  return expr'

evalMatch :: (MonadIO m, RuntimeThrower m) => Match m -> IORef (Env m) -> Eval m (Expr m)
evalMatch Match{enumeral, cases} envRef = do
  tickExpr
  envRef' <- liftIO $ newIORef =<< readIORef envRef
  enumeral' <- eval enumeral envRef'
  case enumeral' of
    Expr'Val (Val'ApiVal (ApiVal'Enumeral e)) -> case e of
      Enumeral name members -> case Map.lookup name cases of
        Nothing -> runtimeThrow RuntimeError'MissingMatchCase
        Just matchCase -> case (matchCase, members) of
          (MatchCase'Tag expr, Nothing) -> eval expr envRef
          (MatchCase'Members var expr, Just _) -> do
            envRef'' <- liftIO $ newIORef =<< readIORef envRef
            addVarToScope envRef'' var enumeral'
            eval expr envRef''
          _ -> runtimeThrow RuntimeError'IncompatibleType -- Should or should have members. Incompatible Val?
    _ -> runtimeThrow RuntimeError'IncompatibleType -- Some enumeral, Incompatible Val?

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
        limit <- variables <$> asks limits
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
    [] -> return $ Expr'Val $ Val'Infer Infer'Null
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
    Expr'Val v -> Eval . ReaderT $ \cfg ->
      apiCall cfg $ ApiCall'Wrap n (Wrap v)
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
  noT <- newIORef notExpr

  eq <- newIORef eqExpr
  neq <- newIORef neqExpr
  lt <- newIORef ltExpr
  lte <- newIORef lteExpr
  gt <- newIORef gtExpr
  gte <- newIORef gteExpr
  concat' <- newIORef concatExpr

  addInt <- newIORef $ intExpr (+)
  addFloat <- newIORef $ floatExpr (+)
  subInt <- newIORef $ intExpr (-)
  subFloat <- newIORef $ floatExpr (-)
  mulInt <- newIORef $ intExpr (*)
  mulFloat <- newIORef $ floatExpr (*)
  divInt <- newIORef $ intExpr (div)
  divFloat <- newIORef $ floatExpr (/)

  tuple <- newIORef tupleExpr

  mapList <- newIORef mapListExpr
  filterList <- newIORef filterListExpr
  reduceList <- newIORef reduceListExpr

  mapOption <- newIORef mapOptionExpr

  mapLeft <- newIORef mapLeftExpr
  mapRight <- newIORef mapRightExpr

  newIORef $ Map.fromList
    [ ("not",noT)

    , ("eq", eq)
    , ("neq", neq)
    , ("lt", lt)
    , ("lte", lte)
    , ("gt", gt)
    , ("gte", gte)

    , ("addInt", addInt)
    , ("addFloat", addFloat)
    , ("subInt", subInt)
    , ("subFloat", subFloat)
    , ("mulInt", mulInt)
    , ("mulFloat", mulFloat)
    , ("divInt", divInt)
    , ("divFloat", divFloat)

    , ("concat", concat')
    , ("tuple", tuple)

    , ("mapOption", mapOption)

    , ("mapList", mapList)
    , ("filterList", filterList)
    , ("reduceList", reduceList)

    , ("mapLeft", mapLeft)
    , ("mapRight", mapRight)
    ]

mapRightExpr :: RuntimeThrower m => Expr m
mapRightExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), expr@(Expr'Val (Val'ApiVal (ApiVal'Enumeral Enumeral{tag,m})))] -> case tag of
      "Right" -> case m >>= Map.lookup "right" of
        Nothing -> runtimeThrow RuntimeError'IncompatibleType -- Not an Either'Left
        Just _ -> do
          left <- f [expr]
          case left of
            Expr'Val v -> return $ Expr'Val $ Val'ApiVal $ ApiVal'Enumeral $ Enumeral tag (Map.insert "right" v <$> m)
            _ -> runtimeThrow RuntimeError'IncompatibleType -- Should be a Val
      "Left" -> return expr
      _ -> runtimeThrow RuntimeError'IncompatibleType -- Not an Either
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments

mapLeftExpr :: RuntimeThrower m => Expr m
mapLeftExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), expr@(Expr'Val (Val'ApiVal (ApiVal'Enumeral Enumeral{tag,m})))] -> case tag of
      "Left" -> case m >>= Map.lookup "left" of
        Nothing -> runtimeThrow RuntimeError'IncompatibleType -- Not an Either'Left
        Just _ -> do
          left <- f [expr]
          case left of
            Expr'Val v -> return $ Expr'Val $ Val'ApiVal $ ApiVal'Enumeral $ Enumeral tag (Map.insert "left" v <$> m)
            _ -> runtimeThrow RuntimeError'IncompatibleType -- Should be a Val
      "Right" -> return expr
      _ -> runtimeThrow RuntimeError'IncompatibleType -- Not an Either
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments

mapOptionExpr :: RuntimeThrower m => Expr m
mapOptionExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn _, Expr'Val (Val'Infer Infer'Null)] -> return $ Expr'Val (Val'Infer Infer'Null)
    [Expr'Fn (Fn f), expr] -> f [expr]
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments

mapListExpr :: RuntimeThrower m => Expr m
mapListExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), Expr'List (List list)] -> go f list
    [Expr'Fn (Fn f), Expr'Val (Val'List list)] -> go f (map Expr'Val list)
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
  where
    go f list = Expr'List . List <$> mapM (f . (:[])) list

filterListExpr :: RuntimeThrower m => Expr m
filterListExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), Expr'List (List list)] -> go f list
    [Expr'Fn (Fn f), Expr'Val (Val'List list)] -> go f (map Expr'Val list)
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
  where
    go f list = Expr'List . List <$>
      filterM
        (\x -> do
          res <- f [x]
          case res of
            Expr'Val (Val'Prim (Prim'Bool b)) -> return b
            _ -> runtimeThrow RuntimeError'IncompatibleType) -- Bool
        list

reduceListExpr :: RuntimeThrower m => Expr m
reduceListExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Fn (Fn f), a, Expr'List (List list)] -> go f a list
    [Expr'Fn (Fn f), a, Expr'Val (Val'List list)] -> go f a (map Expr'Val list)
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
  where
    go f a list = foldlM (\x y -> f [x, y]) a list

intExpr :: RuntimeThrower m => (Int -> Int -> Int) -> Expr m
intExpr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Prim (Prim'Int y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Int y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `op` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Infer (Infer'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `op` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Infer (Infer'Number y))] -> case (toBoundedInteger x, toBoundedInteger y) of
      (Just x', Just y') -> toExpr $ x' `op` y'
      _ -> runtimeThrow RuntimeError'IncompatibleType
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

floatExpr :: RuntimeThrower m => (Double -> Double -> Double) -> Expr m
floatExpr op = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ x `op` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ toRealFloat x `op` y
    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ x `op` toRealFloat y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ toRealFloat x `op` toRealFloat y
    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

boolExpr :: RuntimeThrower m
  => (Scientific -> Scientific -> Bool)
  -> (Int -> Int -> Bool)
  -> (Double -> Double -> Bool)
  -> (Val -> Val -> Bool)
  -> Expr m
boolExpr num int float val = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ x `num` y

    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Prim (Prim'Int y))] -> toExpr $ x `int` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Int y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `int` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Infer (Infer'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `int` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ x `float` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ toRealFloat x `float` y
    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ x `float` toRealFloat y

    [Expr'Val x, Expr'Val y] -> toExpr $ x `val` y

    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

numExpr :: (RuntimeThrower m, ToVal a)
  => (Scientific -> Scientific -> a)
  -> (Int -> Int -> a)
  -> (Double -> Double -> a)
  -> Expr m
numExpr num int float = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ x `num` y

    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Prim (Prim'Int y))] -> toExpr $ x `int` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Int y))] -> case toBoundedInteger x of
      Just x' -> toExpr $ x' `int` y
      Nothing -> runtimeThrow RuntimeError'IncompatibleType
    [Expr'Val (Val'Prim (Prim'Int x)), Expr'Val (Val'Infer (Infer'Number y))] -> case toBoundedInteger y of
      Just y' -> toExpr $ x `int` y'
      Nothing -> runtimeThrow RuntimeError'IncompatibleType

    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ x `float` y
    [Expr'Val (Val'Infer (Infer'Number x)), Expr'Val (Val'Prim (Prim'Float y))] -> toExpr $ toRealFloat x `float` y
    [Expr'Val (Val'Prim (Prim'Float x)), Expr'Val (Val'Infer (Infer'Number y))] -> toExpr $ x `float` toRealFloat y

    (_:_:[]) -> runtimeThrow RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'TooManyArguments
    where
      toExpr v = return $ Expr'Val (toVal v)

notExpr :: RuntimeThrower m => Expr m
notExpr = Expr'Fn . Fn $ \args ->
  case args of
    [] -> runtimeThrow RuntimeError'TooFewArguments
    [Expr'Val (Val'Prim (Prim'Bool x))] -> toExpr $ not x
    _ -> runtimeThrow RuntimeError'TooManyArguments
  where
    toExpr v = return $ Expr'Val (toVal v)

eqExpr :: RuntimeThrower m => Expr m
eqExpr = boolExpr (==) (==) (==) (==)

neqExpr :: RuntimeThrower m => Expr m
neqExpr = boolExpr (/=) (/=) (/=) (/=)

ltExpr :: RuntimeThrower m => Expr m
ltExpr = numExpr (<) (<) (<)

lteExpr :: RuntimeThrower m => Expr m
lteExpr = numExpr (<=) (<=) (<=)

gtExpr :: RuntimeThrower m => Expr m
gtExpr = numExpr (>) (>) (>)

gteExpr :: RuntimeThrower m => Expr m
gteExpr = numExpr (>=) (>=) (>=)

concatExpr :: RuntimeThrower m => Expr m
concatExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    [x, y] -> do
      (u,v) <- case (x,y) of
        (Expr'Val (Val'Prim (Prim'String x')), Expr'Val (Val'Prim (Prim'String y'))) -> return (x',y')
        _ -> runtimeThrow RuntimeError'IncompatibleType -- String
      return $ Expr'Val . Val'Prim . Prim'String $ u `mappend` v
    _ -> runtimeThrow RuntimeError'TooManyArguments

tupleExpr :: RuntimeThrower m => Expr m
tupleExpr = Expr'Fn . Fn $ \args ->
  case args of
    (_:[]) -> runtimeThrow RuntimeError'TooFewArguments
    xs -> return $ Expr'Tuple $ Tuple xs

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
  ApiCall'Wrap n (Wrap w) -> join $ ($ w) <$> Map.lookup n wrap
