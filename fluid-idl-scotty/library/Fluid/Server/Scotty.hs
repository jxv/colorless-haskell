module Fluid.Server.Scotty
  ( runServer
  , respond
  , respondSingleton
  , getSpec
  , ScottyError
  , ScottyT
  , WaiResponse
  , LazyText
  ) where

import qualified Network.Wai as Wai
import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans
import Control.Monad.Trans (lift)
import Fluid.Server (Pull(..), Request, Response, Version, Major, Minor)
import Fluid.Imports
import Fluid.Endpoint

type WaiResponse = Wai.Response

type LazyText = TL.Text

runServer
  :: (MonadIO m, MonadIO n)
  => Pull
  -> (m WaiResponse -> IO WaiResponse)
  -> ScottyT e m ()
  -> n ()
runServer Pull{port} = scottyT port

respond
  :: (ScottyError e, MonadIO m)
  => Pull
  -> ([(LazyText, LazyText)] -> Map Major (Minor, Request -> m (Either Response Response)))
  -> ScottyT e m ()
respond Pull{path} handlerMap = post (literal $ fromText path) $ do
  req <- jsonData
  hdrs <- headers
  resp <- lift $ runFluid (handlerMap hdrs) req
  json resp

respondSingleton
  :: (ScottyError e, MonadIO m)
  => Pull
  -> Version
  -> ([(LazyText, LazyText)] -> Request -> m (Either Response Response))
  -> ScottyT e m ()
respondSingleton Pull{path} version handler = post (literal $ fromText path) $ do
  hdrs <- headers
  req <- jsonData
  resp <- lift $ runFluidSingleton version (handler hdrs) req
  json resp

getSpec :: (ScottyError e, MonadIO m) => Value -> Pull -> ScottyT e m ()
getSpec spec Pull{path} = get (literal $ fromText path) $ json spec
