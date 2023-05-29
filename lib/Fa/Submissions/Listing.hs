-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Submissions.Listing where

import qualified Conduit as C
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.Csv as CSV

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Network.URI (URI)
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Fa.Extractors ((@.), fetchAndScrape, fetchAndScrapePages)
import Fa.Uri (canonicaliseUri)
import Fa.Submissions.Folder (FolderEntry (..))

data SubmissionEntry = SubmissionEntry
  { page :: URI
  , user :: URI
  , thumbnail :: URI
  , kind :: T.Text
  , rating :: T.Text
  , title :: T.Text
  } deriving (Generic, Show, CSV.ToNamedRecord, CSV.DefaultOrdered)

data ListingPageData = ListingPageData
  { submissions :: [SubmissionEntry]
  , folders :: [FolderEntry]
  , nextPage :: Maybe URI
  } deriving (Show)

extractSubmissionEntries :: URI -> Scraper T.Text [SubmissionEntry]
extractSubmissionEntries baseUri =
  chroots ("div" @. "section-body" // "figure") $ do
    [Just page, Just user] <-
      attrs "href" ("figcaption" // "a") <&> map (canonicaliseUri baseUri)
    Just thumbnail <- attr "src" "img" <&> canonicaliseUri baseUri
    title <- attr "title" ("figcaption" // "a")
    [rating, kind] <- attr "class" "figure" <&> T.splitOn " "
    return SubmissionEntry { .. }

extractFolderEntries :: URI -> Scraper T.Text [FolderEntry]
extractFolderEntries baseUri =
  chroots ("div" @. "user-folders" // "a") $ do
    name <- text anySelector
    Just url <- attr "href" anySelector <&> canonicaliseUri baseUri
    return FolderEntry { .. }

extractNextPageUrl :: URI -> Scraper T.Text (Maybe URI)
extractNextPageUrl baseUri =
  (favsPagination <|> galleryPagination) <&> lookup "Next"
  where
    favsPagination :: Scraper T.Text [(T.Text, URI)]
    favsPagination =
      chroots ("div" @. "pagination" // "a") $ do
        label <- text "a"
        Just target <- attr "href" "a" <&> canonicaliseUri baseUri
        return (label, target)

    galleryPagination :: Scraper T.Text [(T.Text, URI)]
    galleryPagination =
      chroots ("div" @. "submission-list" // "form") $ do
        label <- text "button"
        Just target <- attr "action" "form" <&> canonicaliseUri baseUri
        return (label, target)

extractListingPageData :: URI -> Scraper T.Text ListingPageData
extractListingPageData baseUri = do
  submissions <- extractSubmissionEntries baseUri
  folders <- extractFolderEntries baseUri
  nextPage <- extractNextPageUrl baseUri
  return ListingPageData { .. }

scrapeListingPage :: HTTP.Manager -> URI -> IO (Maybe ListingPageData)
scrapeListingPage client uri =
  fetchAndScrape client (extractListingPageData uri) uri

scrapeListingPages ::
  HTTP.Manager -> URI -> C.ConduitT () ListingPageData IO ()
scrapeListingPages client =
  fetchAndScrapePages client extractListingPageData nextPage
