{-# LANGUAGE DeriveGeneric #-}
module Colorless.Runtime
  ( emptyEnv
  , runEval
  , eval
  , Eval
  , EvalConfig(..)
  , Val(..)
  , Atom(..)
  , Begin(..)
  , Define(..)
  , If(..)
  , Call(..)
  , Lambda(..)
  ) where

import qualified Data.HashMap.Lazy as HML
import qualified Data.Map as Map
import qualified Data.Vector as V

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks, lift)
import Control.Applicative ((<|>))
import Control.Monad (mzero, when)
import Data.Aeson
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Aeson.Types (parseMaybe, Value)
import GHC.Generics

import Colorless.Types

data EvalConfig m = EvalConfig
  { options :: Options
  , apiCall :: Value -> m Value
  }

newtype Eval m a = Eval (ReaderT (EvalConfig m) m a)
  deriving (Functor, Applicative, Monad, MonadReader (EvalConfig m), MonadIO)

instance RuntimeThrower m => RuntimeThrower (Eval m) where
  runtimeThrow err = Eval (lift $ runtimeThrow err)

getOptions :: Monad m => Eval m Options
getOptions = asks options

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


runEval :: MonadIO m => Eval m a -> EvalConfig m -> m a
runEval (Eval r) = runReaderT r

type Env m = Map Text (IORef (Val m))

data Val m
  = Val'Atom Atom
  | Val'Record (Record m)
  | Val'Value Value
  | Val'If (If m)
  | Val'Set (Set m)
  | Val'Get (Get m)
  | Val'Define (Define m)
  | Val'Lambda (Lambda m)
  | Val'List (List m)
  | Val'Fn (Fn m)
  | Val'Call (Call m)
  | Val'Begin (Begin m)
  deriving (Show, Eq)

instance  FromJSON (Val m) where
  parseJSON v
    =   (Val'Atom <$> parseJSON v)
    <|> (Val'List <$> parseJSON v)
    <|> (Val'Record <$> parseJSON v)
    <|> (Val'Begin <$> parseJSON v)
    <|> (Val'Set <$> parseJSON v)
    <|> (Val'Get <$> parseJSON v)
    <|> (Val'Define <$> parseJSON v)
    <|> (Val'Lambda <$> parseJSON v)
    <|> (Val'If <$> parseJSON v)
    <|> (Val'Call <$> parseJSON v)
    <|> (pure $ Val'Value v)

instance ToJSON (Val m) where
  toJSON = \case
    Val'Atom atom -> toJSON atom
    Val'Record r -> toJSON r
    Val'Value v -> v
    Val'If i -> toJSON i
    Val'Set s -> toJSON s
    Val'Get g -> toJSON g
    Val'Define d -> toJSON d
    Val'Lambda l -> toJSON l
    Val'List l -> toJSON l
    Val'Fn _ -> error "no to json for function"
    Val'Call c -> toJSON c
    Val'Begin b -> toJSON b

data Enumerator m = Enumerator
  { e :: Text
  , m :: Maybe (Map Text (Val m))
  } deriving (Show, Eq, Generic)

instance FromJSON (Enumerator m) where
  parseJSON (Object o) = do
    tag <- parseJSON =<< o .: "e"

    maybeMembers <- do
      case HML.lookup "m" o of
        Nothing -> return Nothing
        Just m -> do
          members <- parseJSON m
          m' <- mapM parseJSON members
          return $ Just m'

    Enumerator tag <$> pure maybeMembers
  parseJSON _ = mzero

instance ToJSON (Enumerator m) where
  toJSON Enumerator{e,m} = object $ [ "e" .= e ] ++ m'
    where
      m' = maybe [] (\x -> [ "m" .= x ]) m

data Record m = Record
  { n :: Text
  , m :: Maybe (Map Text (Val m))
  , e :: Maybe (Enumerator m)
  } deriving (Show, Eq, Generic)

instance FromJSON (Record m) where
  parseJSON (Object o) = do
    name <- parseJSON =<< o .: "n"

    maybeMembers <- do
      case HML.lookup "m" o of
        Nothing -> return Nothing
        Just m -> do
          members <- parseJSON m
          m' <- mapM parseJSON members
          return $ Just m'

    maybeEnum <- do
      case HML.lookup "e" o of
        Nothing -> return Nothing
        Just e -> Just <$> parseJSON e

    Record name <$> pure maybeMembers <*> pure maybeEnum
  parseJSON _ = mzero

instance ToJSON (Record m) where
  toJSON Record{n,m,e} = object $ [ "n" .= n ] ++ m' ++ e'
    where
      m' = maybe [] (\x -> ["m" .= x]) m
      e' = maybe [] (\x -> ["e" .= x]) e

data Atom = Atom
  { symbol :: Text
  } deriving (Show, Eq)

instance ToJSON Atom where
  toJSON Atom{symbol} = object [ "@" .= symbol ]

instance FromJSON Atom where
  parseJSON (Object o) = Atom <$> o .: "@"
  parseJSON _ = mzero

data If m = If
  { cond :: Val m
  , true :: Val m
  , false :: Val m
  } deriving (Show, Eq, Generic)

instance ToJSON (If m) where
  toJSON If{cond,true,false} = toJSON ["if", toJSON cond, toJSON true, toJSON false]

instance FromJSON (If m) where
  parseJSON (Array arr) = case V.toList arr of
    ["if", cond, true, false] -> If <$> parseJSON cond <*> parseJSON true <*> parseJSON false
    _ -> mzero
  parseJSON _ = mzero

data Set m = Set
  { var :: Text
  , expr :: Val m
  } deriving (Show, Eq, Generic)

instance ToJSON (Set m) where
  toJSON Set{var,expr} = toJSON ["set", toJSON var, toJSON expr]

instance FromJSON (Set m) where
  parseJSON (Array arr) = case V.toList arr of
    ["set", var, expr] -> Set <$> parseJSON var <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

data Get m = Get
  { path :: [Text]
  , val :: Val m
  } deriving (Show, Eq, Generic)

instance ToJSON (Get m) where
  toJSON Get{path, val} = toJSON ["get", toJSON path, toJSON val]

instance FromJSON (Get m) where
  parseJSON (Array arr) = case V.toList arr of
    ["get", path, val] -> Get <$> parseJSON path <*> parseJSON val
    _ -> mzero
  parseJSON _ = mzero

data Define m = Define
  { var :: Text
  , expr :: Val m
  } deriving (Show, Eq, Generic)

instance ToJSON (Define m) where
  toJSON Define{var,expr} = toJSON ["def", toJSON var, toJSON expr]

instance FromJSON (Define m) where
  parseJSON (Array arr) = case V.toList arr of
    ["def", var, expr] -> Define <$> parseJSON var <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

data Lambda m = Lambda
  { params :: [Map Text (Val m)]
  , expr :: Val m
  } deriving (Show, Eq, Generic)

instance FromJSON (Lambda m) where
  parseJSON (Array arr) = case V.toList arr of
    ["fn", params, expr] -> Lambda <$> parseJSON params <*> parseJSON expr
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON (Lambda m) where
  toJSON Lambda{params,expr} = toJSON ["fn", toJSON params, toJSON expr]

data Call m = Call
  { fn :: Val m
  , args :: [Val m]
  } deriving (Show, Eq, Generic)

instance FromJSON (Call m) where
  parseJSON (Array arr) = case V.toList arr of
    (x:xs) -> Call <$> parseJSON x <*> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON (Call m) where
  toJSON Call{fn, args} = toJSON $ fn : args

data List m = List
  { list :: [Val m]
  } deriving (Show, Eq, Generic)

instance FromJSON (List m) where
  parseJSON (Object o) = List <$> o .: "List"
  parseJSON _ = mzero

instance ToJSON (List m) where
  toJSON List{list} = object [ "List" .= toJSON list ]

newtype Fn m = Fn ([Val m] -> Eval m (Val m))

instance Show (Fn m) where show _ = "<Fn>"

instance Eq (Fn m) where _ == _ = False

data Begin m = Begin
  { vals :: [Val m]
  } deriving (Show, Eq)

instance FromJSON (Begin m) where
  parseJSON (Array arr) = case V.toList arr of
    "begin":xs -> Begin <$> mapM parseJSON xs
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON (Begin m) where
  toJSON Begin{vals} = toJSON $ "begin" : map toJSON vals

eval :: (MonadIO m, RuntimeThrower m) => Val m -> IORef (Env m) -> Eval m (Val m)
eval val envRef = case val of
  Val'Atom atom -> evalAtom atom envRef
  Val'Value v -> evalValue v envRef
  Val'If if' -> evalIf if' envRef
  Val'Set set -> evalSet set envRef
  Val'Get get -> evalGet get envRef
  Val'Define define -> evalDefine define envRef
  Val'Lambda lambda -> evalLambda lambda envRef
  Val'Fn _ -> return val -- TODO: Should eval?
  Val'List list -> evalList list envRef
  Val'Record r -> evalRecord r envRef
  Val'Call call -> evalCall call envRef
  Val'Begin vals -> evalBegin vals envRef

varLookup :: (MonadIO m, RuntimeThrower m) => Map Text (IORef a) -> Text -> m a
varLookup env symbol = case Map.lookup symbol env of
  Nothing -> runtimeThrow $ RuntimeError'UnknownVariable symbol
  Just var -> liftIO $ readIORef $ var

evalAtom :: (MonadIO m, RuntimeThrower m) => Atom -> IORef (Env m) -> Eval m (Val m)
evalAtom Atom{symbol} envRef = do
  env <- liftIO $ readIORef envRef
  varLookup env symbol

evalValue :: (MonadIO m, RuntimeThrower m) => Value -> IORef (Env m) -> Eval m (Val m)
evalValue v envRef = case v of
  Null -> unchanged
  Bool _ -> unchanged
  String _ -> unchanged
  Number _ -> unchanged
  Array _ -> unchanged
  Object o -> do
    o' <- mapM
      (\val -> let
        val' = fromJust $ parseMaybe parseJSON val -- should NEVER fail as it will ALWAYS defer to the input Value
        in eval val' envRef)
      o
    return $ Val'Value (toJSON o')
  where
    unchanged = return $ Val'Value v

evalIf :: (MonadIO m, RuntimeThrower m) => If m -> IORef (Env m) -> Eval m (Val m)
evalIf If{cond, true, false} envRef = do
  envRef' <- liftIO $ newIORef =<< readIORef envRef
  v <- eval cond envRef'
  case v of
    Val'Value (Bool cond') -> do
      envRef'' <- liftIO $ newIORef =<< readIORef envRef
      eval (if cond' then true else false) envRef''
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalSet :: (MonadIO m, RuntimeThrower m) => Set m -> IORef (Env m) -> Eval m (Val m)
evalSet Set{var, expr} envRef = do
  expr' <- eval expr envRef
  env <- liftIO $ readIORef envRef
  case Map.lookup var env of
    Nothing -> runtimeThrow $ RuntimeError'UnknownVariable var
    Just v -> do
      liftIO $ writeIORef v expr'
      return expr'

evalGet :: (MonadIO m, RuntimeThrower m) => Get m -> IORef (Env m) -> Eval m (Val m)
evalGet Get{path, val} envRef = do
  Val'Value v <- eval val envRef
  return . Val'Value $ foldl' (\(Object v') idx -> v' HML.! idx) v path

evalDefine :: (MonadIO m, RuntimeThrower m) => Define m -> IORef (Env m) -> Eval m (Val m)
evalDefine Define{var, expr} envRef = do
  expr' <- eval expr envRef
  env <- liftIO $ readIORef envRef
  ref <- liftIO $ newIORef expr'
  limit <- variableLimit <$> getOptions
  addVarToEnv limit envRef var ref env
  return expr'

evalLambda :: (MonadIO m, RuntimeThrower m) => Lambda m -> IORef (Env m) -> Eval m (Val m)
evalLambda Lambda{params, expr} envRef = do
  return . Val'Fn . Fn $ \vals -> do
    let keys = map (head . Map.keys) params -- TODO: Check types
    let args = Map.fromList $ zip keys vals -- TODO: Same length
    let keysLen = length keys
    let argsLen = length args
    if keysLen /= argsLen
      then runtimeThrow $ if keysLen < argsLen
        then RuntimeError'TooManyArguments
        else RuntimeError'TooFewArguments
      else do
        args' <- liftIO $ mapM newIORef args
        limit <- variableLimit <$> getOptions
        envRef' <- addEnvToEnv limit args' envRef
        eval expr envRef'

evalList :: (MonadIO m, RuntimeThrower m) => List m -> IORef (Env m)-> Eval m (Val m)
evalList List{list} envRef = do
  list' <- mapM (flip eval envRef) list
  return . Val'List $ List list'

evalRecord :: (MonadIO m, RuntimeThrower m) => Record m -> IORef (Env m) -> Eval m (Val m)
evalRecord Record{n,m,e} envRef = do
  m' <- case m of
    Nothing -> return Nothing
    Just members -> Just <$> mapM (flip eval envRef) members
  let r = Record n m' e
  Eval . ReaderT $ \cfg -> do
    v <- apiCall cfg (toJSON r)
    return $ Val'Value v

evalCall :: (MonadIO m, RuntimeThrower m) => Call m -> IORef (Env m) -> Eval m (Val m)
evalCall Call{fn, args} envRef = do
  val <- eval fn envRef
  case val of
    Val'Fn (Fn fn') -> do
      args' <- mapM (flip eval envRef) args
      fn' args'
    Val'Atom Atom{symbol} -> do
      env <- liftIO $ readIORef envRef
      v <- varLookup env symbol
      case v of
        Val'Fn (Fn fn') -> do
          args' <- mapM (flip eval envRef) args
          fn' args'
        _ -> runtimeThrow $ RuntimeError'IncompatibleType
    Val'Value (String symbol) -> do
      env <- liftIO $ readIORef envRef
      v <- varLookup env symbol
      case v of
        Val'Fn (Fn fn') -> do
          args' <- mapM (flip eval envRef) args
          fn' args'
        _ -> runtimeThrow $ RuntimeError'IncompatibleType
    _ -> runtimeThrow RuntimeError'IncompatibleType

evalBegin :: (MonadIO m, RuntimeThrower m) => Begin m -> IORef (Env m) -> Eval m (Val m)
evalBegin Begin{vals} envRef = case vals of
  [] -> return $ Val'Value Null
  _ -> last <$> mapM (flip eval envRef) vals

emptyEnv :: RuntimeThrower m => IO (IORef (Env m))
emptyEnv = do
  eq <- newIORef $ Val'Fn . Fn $ \[x, y] -> return . Val'Value $ Bool (x == y)

  neq <- newIORef $ Val'Fn . Fn $ \[x, y] -> return . Val'Value $ Bool (x /= y)

  add <- newIORef $ Val'Fn . Fn $ \vals -> case vals of
    [Val'Value (Number x), Val'Value (Number y)] -> return . Val'Value $ Number (x + y)
    _ -> runtimeThrow RuntimeError'IncompatibleType

  sub <- newIORef $ Val'Fn . Fn $ \vals -> case vals of
    [Val'Value (Number x), Val'Value (Number y)] -> return . Val'Value $ Number (x - y)
    _ -> runtimeThrow RuntimeError'IncompatibleType

  mul <- newIORef $ Val'Fn . Fn $ \vals -> case vals of
    [Val'Value (Number x), Val'Value (Number y)] -> return . Val'Value $ Number (x * y)
    _ -> runtimeThrow RuntimeError'IncompatibleType

  div' <- newIORef $ Val'Fn . Fn $ \vals -> case vals of
    [Val'Value (Number x), Val'Value (Number y)] -> return . Val'Value $ Number (x / y)
    _ -> runtimeThrow RuntimeError'IncompatibleType

  concat' <- newIORef $ Val'Fn . Fn $ \vals -> case vals of
    [Val'Value (String x), Val'Value (String y)] -> return . Val'Value $ String (x `mappend` y)
    _ -> runtimeThrow RuntimeError'IncompatibleType

  newIORef $ Map.fromList
    [ ("==", eq)
    , ("!=", neq)
    , ("+", add)
    , ("-", sub)
    , ("*", mul)
    , ("/", div')
    , ("concat", concat')
    ]
