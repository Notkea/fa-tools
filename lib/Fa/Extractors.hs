-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Extractors where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as LBS
import qualified Network.URI as U
import qualified Network.HTTP.Client as HTTP
import qualified Conduit as C
import qualified Text.HTML.Scalpel as S

import Data.Functor ((<&>))
import Fa.Uri (uriString, canonicaliseUri)

infixl 6 @.
(@.) :: S.TagName -> String -> S.Selector
(@.) tag className = tag S.@: [S.hasClass className]

infixl 6 @#
(@#) :: S.TagName -> String -> S.Selector
(@#) tag idName = tag S.@: ["id" S.@= idName]

link :: U.URI -> String -> S.Selector -> S.Scraper T.Text (Maybe U.URI)
link baseUri attr sel = S.attr attr sel <&> canonicaliseUri baseUri

links :: U.URI -> String -> S.Selector -> S.Scraper T.Text [Maybe U.URI]
links baseUri attr sel = S.attrs attr sel <&> map (canonicaliseUri baseUri)

hasMatch :: S.Selector -> S.Scraper T.Text Bool
hasMatch sel = S.htmls sel <&> not . null

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

fetchAndScrapePages ::
     HTTP.Manager
  -> (U.URI -> S.Scraper T.Text a)
  -> (a -> Maybe U.URI)
  -> U.URI
  -> C.ConduitT () a IO ()
fetchAndScrapePages client scraper nextPage uri = do
  Just currentPageData <- C.lift $ fetchAndScrape client (scraper uri) uri
  C.yield currentPageData
  maybe mempty
    (fetchAndScrapePages client scraper nextPage)
    (nextPage currentPageData)
