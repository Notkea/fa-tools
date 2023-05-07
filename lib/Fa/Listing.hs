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
import Data.Default (def)
import Fa.Client (canonicaliseUri, uriString)

-- TODO: use a distinct enriched type for URI
instance CSV.ToField URI where
  toField = CSV.toField . uriString

data SubmissionEntry = SubmissionEntry
  { page :: URI
  , thumbnail :: URI
  , entryType :: T.Text
  , rating :: T.Text
  , title :: T.Text
  } deriving (Generic, Show, CSV.ToNamedRecord, CSV.DefaultOrdered)

data FolderEntry = FolderEntry
  { name :: T.Text
  , url :: URI
  } deriving (Show)

data ListingPageData = ListingPageData
  { submissions :: [SubmissionEntry]
  , folders :: [FolderEntry]
  , nextPage :: Maybe URI
  } deriving (Show)

extractSubmissionEntries :: URI -> Scraper T.Text [SubmissionEntry]
extractSubmissionEntries baseUri =
  chroots ("section" @: ["id" @= "gallery-gallery"] // "figure") $ do
    Just page <- attr "href" ("figcaption" // "a") <&> canonicaliseUri baseUri
    Just thumbnail <- attr "src" "img" <&> canonicaliseUri baseUri
    title <- attr "title" ("figcaption" // "a")
    [rating, entryType] <- attr "class" "figure" <&> T.splitOn " "
    return SubmissionEntry { .. }

extractFolderEntries :: URI -> Scraper T.Text [FolderEntry]
extractFolderEntries baseUri =
  chroots ("div" @: [hasClass "user-folders"] // "a") $ do
    name <- text anySelector
    Just url <- attr "href" anySelector <&> canonicaliseUri baseUri
    return FolderEntry { .. }

extractNextPageUrl :: URI -> Scraper T.Text (Maybe URI)
extractNextPageUrl baseUri =
  fmap (lookup "Next") $
    chroots ("div" @: [hasClass "submission-list"] // "form") $ do
      label <- text "button"
      Just target <- attr "action" "form" <&> canonicaliseUri baseUri
      return (label, target)

extractListingPageData :: URI -> Scraper T.Text ListingPageData
extractListingPageData baseUri = do
  submissions <- extractSubmissionEntries baseUri
  folders <- extractFolderEntries baseUri
  nextPage <- extractNextPageUrl baseUri
  return ListingPageData { .. }

scrapListingDataMultiPage :: HTTP.Manager -> URI -> IO [ListingPageData]
scrapListingDataMultiPage client uri = do
  Just currentPageData <- scrapePage uri
  nextPagesData <- scrapNextPage (nextPage currentPageData)
  return $ currentPageData : nextPagesData

  where
    fetchAndScrape :: Scraper T.Text a -> URL -> IO (Maybe a)
    fetchAndScrape =
      flip $ scrapeURLWithConfig (def { S.manager = Just client })

    scrapePage :: URI -> IO (Maybe ListingPageData)
    scrapePage page =
      fetchAndScrape (extractListingPageData page) (uriString page)

    scrapNextPage :: Maybe URI -> IO [ListingPageData]
    scrapNextPage (Just nextPage) = scrapListingDataMultiPage client nextPage
    scrapNextPage Nothing = return []
