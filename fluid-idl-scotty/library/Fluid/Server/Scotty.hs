module Fluid.Server.Scotty
  ( runServer
  , sendResponse
  , sendResponseSingleton
  , getSpec
  , ScottyError
  , ScottyT
  , WaiResponse
  ) where

import qualified Network.Wai as Wai
import Web.Scotty.Trans
import Control.Monad.Trans (lift)
import Fluid.Server (Pull(..), Request, Response, Version, Major, Minor)
import Fluid.Imports
import Fluid.Endpoint

type WaiResponse = Wai.Response

runServer
  :: (MonadIO m, MonadIO n)
  => Pull
  -> (m WaiResponse -> IO WaiResponse)
  -> ScottyT e m ()
  -> n ()
runServer Pull{port} = scottyT port

sendResponse
  :: (ScottyError e, MonadIO m)
  => Pull
  -> Map Major (Minor, Request -> m (Either Response Response))
  -> ScottyT e m ()
sendResponse Pull{path} handlerMap = post (literal $ fromText path) $ do
  req <- jsonData
  resp <- lift $ runFluid handlerMap req
  json resp

sendResponseSingleton
  :: (ScottyError e, MonadIO m)
  => Pull
  -> Version
  -> (Request -> m (Either Response Response))
  -> ScottyT e m ()
sendResponseSingleton Pull{path} version handler = post (literal $ fromText path) $ do
  hdrs <- headers
  req <- jsonData
  resp <- lift $ runFluidSingleton version handler req
  json resp

getSpec :: (ScottyError e, MonadIO m) => Value -> Pull -> ScottyT e m ()
getSpec spec Pull{path} = get (literal $ fromText path) $ json spec
