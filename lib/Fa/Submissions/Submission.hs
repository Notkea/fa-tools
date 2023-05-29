-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Submissions.Submission where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Network.URI (URI)
import Data.Functor ((<&>))
import Fa.Submissions.Folder (FolderEntry (..))
import Fa.Client ((@.), fetchAndScrape)
import Fa.Uri (canonicaliseUri)
import Fa.Date (extractAbsDate)

type HTML = T.Text

data Submission = Submission
  { page :: URI
  , download :: URI
  , author :: T.Text
  , date :: T.Text
  , tags :: [T.Text]
  , folders :: [FolderEntry]
  , title :: T.Text
  , description :: HTML
  } deriving (Generic, Show, ToJSON)

extractTags :: Scraper T.Text [T.Text]
extractTags = texts ("section" @. "tags-row" // "a")

extractFolderEntries :: URI -> Scraper T.Text [FolderEntry]
extractFolderEntries baseUri =
  chroots ("section" @. "folder-list-container" // "a") $ do
    Just url <- attr "href" "a" <&> canonicaliseUri baseUri
    name <- text "span"  -- TODO: also get the parent folder?
    return FolderEntry { .. }

extractSubmission :: URI -> Scraper T.Text Submission
extractSubmission page = do
  Just download <- attr "href" ("div" @. "download" // "a") <&> canonUri
  title <- text $ metaContainer // "div" @. "submission-title" // "p"
  author <- text $ metaContainer // "a" // "strong"
  Just date <- extractAbsDate metaContainer
  description <- html $ "div" @. "submission-description"
  tags <- extractTags
  folders <- extractFolderEntries page
  return Submission { .. }
  where
    metaContainer :: Selector
    metaContainer = "div" @. "submission-id-sub-container"

    canonUri :: T.Text -> Maybe URI
    canonUri = canonicaliseUri page

scrapeSubmission :: HTTP.Manager -> URI -> IO (Maybe Submission)
scrapeSubmission client uri = fetchAndScrape client (extractSubmission uri) uri