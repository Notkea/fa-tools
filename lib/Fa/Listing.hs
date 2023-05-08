-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Listing where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.Csv as CSV

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Network.URI (URI)
import Data.Functor ((<&>))
import Fa.Client ((@.), (@#), fetchAndScrape)
import Fa.Uri (canonicaliseUri)
import Fa.Folder (FolderEntry (..))

data SubmissionEntry = SubmissionEntry
  { page :: URI
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
  chroots ("section" @# "gallery-gallery" // "figure") $ do
    Just page <- attr "href" ("figcaption" // "a") <&> canonicaliseUri baseUri
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
  fmap (lookup "Next") $
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

scrapeListingDataMultiPage :: HTTP.Manager -> URI -> IO [ListingPageData]
scrapeListingDataMultiPage client uri = do
  Just currentPageData <- scrapePage uri
  nextPagesData <- scrapNextPage (nextPage currentPageData)
  return $ currentPageData : nextPagesData
  where
    scrapePage :: URI -> IO (Maybe ListingPageData)
    scrapePage page = fetchAndScrape client (extractListingPageData page) page

    scrapNextPage :: Maybe URI -> IO [ListingPageData]
    scrapNextPage (Just nextPage) = scrapeListingDataMultiPage client nextPage
    scrapNextPage Nothing = return []
