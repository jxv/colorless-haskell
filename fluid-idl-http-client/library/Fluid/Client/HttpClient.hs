module Fluid.Client.HttpClient
  ( sendRequest
  , Manager
  , RequestHeaders
  , HttpClientResponse
  ) where

import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Client hiding (Response, Request)
import Network.HTTP.Types.Header (RequestHeaders)
import Data.Aeson (ToJSON(..), encode, FromJSON, decode)
import Data.ByteString.Lazy.Char8 as BL (ByteString)
import Data.Text.Conversions (fromText)

import Fluid.Client
import Fluid.Ast (ToAst)

type HttpClientResponse = HttpClient.Response

sendRequest
  :: (HasType meta, ToVal meta, ToAst a, HasType a, FromVal a, HasType err, FromVal err)
  => Manager
  -> Pull
  -> RequestHeaders
  -> Request meta a
  -> IO (HttpClientResponse BL.ByteString, Maybe (Response err a))
sendRequest manager pull headers req = do
  initialRequest <- parseRequest (fromText $ pullAddress pull)
  let request = initialRequest
        { method = "POST"
        , requestHeaders = [("Content-Type","application/json")] ++ headers
        , requestBody = RequestBodyLBS $ encode req
        }
  resp <- httpLbs request manager
  return (resp, decode $ responseBody resp)
