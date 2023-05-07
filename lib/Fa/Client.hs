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

import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)

type RequestModifier = HTTP.Request -> IO HTTP.Request

addingHeaders :: HTTP.RequestHeaders -> RequestModifier
addingHeaders headers req = do
  req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
  return $ req' {
    HTTP.requestHeaders = headers ++ HTTP.requestHeaders req'
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
parseHeaderLines = T.lines >>> map (T.splitOn ": ") >>> mapMaybe toHeader
  where
    toHeader :: [T.Text] -> Maybe HTTP.Header
    toHeader [key, val] = Just (CI.mk $ T.encodeUtf8 key, T.encodeUtf8 val)
    toHeader _ = Nothing

canonicaliseUri :: U.URI -> T.Text -> Maybe U.URI
canonicaliseUri baseUri relativeRef =
  (`U.relativeTo` baseUri) <$> U.parseRelativeReference (T.unpack relativeRef)

uriString :: U.URI -> String
uriString = flip (U.uriToString id) ""
