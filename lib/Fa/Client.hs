-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Client where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Network.URI as U
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Conduit as HTTPC
import qualified Conduit as C
import qualified Text.HTML.Scalpel as S

import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (runResourceT)

import Fa.Uri

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

fetchAndScrape :: HTTP.Manager -> S.Scraper T.Text a -> U.URI -> IO (Maybe a)
fetchAndScrape client = flip (S.scrapeURLWithConfig scalpelCfg . uriString)
  where
    scalpelCfg :: S.Config T.Text
    scalpelCfg = S.Config { S.manager = Just client, S.decoder = decoder }

    -- FA pages are encoded as UTF-8, but may contain Windows-1252 story
    -- previews. To avoid simply failing, we need the decoding to be more
    -- lenient.
    decoder :: S.Decoder T.Text
    decoder =
      T.decodeUtf8With T.lenientDecode . LBS.toStrict . HTTP.responseBody

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

sinkFor :: FilePath -> ByteSink
sinkFor "-" = C.stdoutC
sinkFor path = C.sinkFile path
