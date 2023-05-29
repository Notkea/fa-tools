-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Client where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import qualified Network.URI as U
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Conduit as HTTPC
import qualified Conduit as C

import Control.Arrow ((>>>))
import Control.Exception (catch, throwIO)
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (runResourceT)
import Fa.Uri (uriString)

type RequestModifier = HTTP.Request -> IO HTTP.Request

addingHeaders :: HTTP.RequestHeaders -> RequestModifier
addingHeaders headers req = do
  req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
  return $ req' {
    HTTP.requestHeaders = nub $ headers ++ HTTP.requestHeaders req'
  }

newHttpManagerWithSession :: HTTP.RequestHeaders -> IO HTTP.Manager
newHttpManagerWithSession headers = HTTP.newManager $ HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = addingHeaders headers
}

filterSessionHeaders :: HTTP.RequestHeaders -> HTTP.RequestHeaders
filterSessionHeaders = filter (flip notElem excludedHeaders . fst)
  where
    excludedHeaders :: [HTTP.HeaderName]
    excludedHeaders = map (CI.mk . T.encodeUtf8)
      [ "host"
      , "connect"
      ]

parseHeaderLines :: T.Text -> HTTP.RequestHeaders
parseHeaderLines = T.lines >>> map unpackEntry >>> mapMaybe toHeader
  where
    unpackEntry :: T.Text -> [ByteString]
    unpackEntry = T.splitOn ":" >>> map (T.encodeUtf8 . T.strip)

    toHeader :: [ByteString] -> Maybe HTTP.Header
    toHeader [key, val] = Just (CI.mk key, val)
    toHeader _ = Nothing

simplifyHttpError :: IO () -> IO ()
simplifyHttpError m = catch m handleErr
  where
    handleErr :: HTTP.HttpException -> IO ()
    handleErr (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException res _)) =
      fail $ formatResponseStatus $ HTTP.responseStatus res
    handleErr x = throwIO x

    formatResponseStatus :: HTTP.Status -> String
    formatResponseStatus HTTP.Status { statusCode, statusMessage } =
      show statusCode ++ ": " ++ show statusMessage

type ByteSink = C.ConduitT ByteString C.Void (C.ResourceT IO) ()

downloadStream :: HTTP.Manager -> U.URI -> ByteSink -> IO ()
downloadStream client uri sink = simplifyHttpError $ do
  request <- HTTP.parseUrlThrow $ uriString uri
  runResourceT $ do
    response <- HTTPC.http request client
    C.connect (HTTPC.responseBody response) sink

sinkFor :: FilePath -> ByteSink
sinkFor "-" = C.stdoutC
sinkFor path = C.sinkFile path
