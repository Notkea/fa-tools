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
import qualified Network.HTTP.Conduit as HTTPC
import qualified Conduit as C
import qualified Text.HTML.Scalpel as S

import Control.Arrow ((>>>))
import Data.Default (def)
import Data.Maybe (mapMaybe)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (runResourceT)
import System.FilePath.Posix (takeBaseName)

import Fa.Uri

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

fetchAndScrape :: HTTP.Manager -> S.Scraper T.Text a -> U.URI -> IO (Maybe a)
fetchAndScrape client = flip (S.scrapeURLWithConfig scalpelCfg . uriString)
  where
    scalpelCfg :: S.Config T.Text
    scalpelCfg = def { S.manager = Just client }

infixl 6 @.
(@.) :: S.TagName -> String -> S.Selector
(@.) tag className = tag S.@: [S.hasClass className]

infixl 6 @#
(@#) :: S.TagName -> String -> S.Selector
(@#) tag idName = tag S.@: ["id" S.@= idName]

type ByteSink = C.ConduitT ByteString C.Void (C.ResourceT IO) ()

downloadStream :: HTTP.Manager -> U.URI -> ByteSink -> IO ()
downloadStream client uri sink = do
  request <- HTTP.parseRequest $ uriString uri
  runResourceT $ do
    response <- HTTPC.http request client
    C.connect (HTTPC.responseBody response) sink

sinkFor :: U.URI -> Maybe FilePath -> ByteSink
sinkFor _ (Just "-") = C.stdoutC
sinkFor _ (Just path) = C.sinkFile path
sinkFor uri Nothing = C.sinkFile $ takeBaseName $ uriString uri
