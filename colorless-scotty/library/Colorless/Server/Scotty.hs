module Colorless.Server.Scotty
  ( sendResponse
  , sendResponseSingleton
  , getSpec
  ) where

import Web.Scotty.Trans
import Control.Monad.Trans (lift)
import Colorless.Server (Pull(..), RuntimeThrower, Request, Response, Version, Major, Minor)
import Colorless.Imports
import Colorless.Endpoint

sendResponse
  :: (ScottyError e, MonadIO m, RuntimeThrower m)
  => Pull
  -> Map Major (Minor, Request -> m Response)
  -> ScottyT e m ()
sendResponse Pull{path} handlerMap = post (literal $ fromText path) $ do
  req <- jsonData
  resp <- lift $ runColorless handlerMap req
  json resp

sendResponseSingleton
  :: (ScottyError e, MonadIO m, RuntimeThrower m)
  => Pull
  -> Version
  -> (Request -> m Response)
  -> ScottyT e m ()
sendResponseSingleton Pull{path} version handler = post (literal $ fromText path) $ do
  req <- jsonData
  resp <- lift $ runColorlessSingleton version handler req
  json resp

getSpec :: (ScottyError e, MonadIO m) => Value -> Pull -> ScottyT e m ()
getSpec spec Pull{path} = get (literal $ fromText path) $ json spec
