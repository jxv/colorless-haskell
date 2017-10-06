module Colorless.Client.HttpClient
  ( sendRequest
  ) where

import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Client hiding (Response, Request)
import Network.HTTP.Types.Header (RequestHeaders)
import Data.Aeson (ToJSON(..), encode, FromJSON, decode)
import Data.ByteString.Lazy.Char8 as BL (ByteString)
import Data.Text.Conversions (fromText)

import Colorless.Client
import Colorless.Ast (ToAst)

sendRequest
  :: (HasType meta, ToJSON meta, ToAst a, HasType a, FromJSON a, HasType err, FromJSON err)
  => Manager
  -> Pull
  -> RequestHeaders
  -> Request meta a
  -> IO (HttpClient.Response BL.ByteString, Maybe (Response err a))
sendRequest manager pull headers req = do
  initialRequest <- parseRequest (fromText $ pullAddress pull)
  let request = initialRequest
        { method = "POST"
        , requestHeaders = [("Content-Type","application/json")] ++ headers
        , requestBody = RequestBodyLBS $ encode req
        }
  resp <- httpLbs request manager
  return (resp, decode $ responseBody resp)
