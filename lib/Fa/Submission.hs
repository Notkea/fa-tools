-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Submission where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Network.URI (URI)
import Data.Functor ((<&>))
import Fa.Folder (FolderEntry (..))
import Fa.Client ((@.), canonicaliseUri, fetchAndScrape)

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
  , inlineWriting :: Maybe HTML  -- pre-rendered BBCode
  } deriving (Generic, Show, ToJSON)

extractTags :: Scraper T.Text [T.Text]
extractTags = texts ("section" @. "tags-row" // "a")

extractFolderEntries :: URI -> Scraper T.Text [FolderEntry]
extractFolderEntries baseUri =
  chroots ("section" @. "folder-list-container" // "a") $ do
    Just url <- attr "href" "a" <&> canonicaliseUri baseUri
    name <- text "span"  -- TODO: also get the parent folder?
    return FolderEntry { .. }

extractInlineWriting :: Scraper T.Text (Maybe HTML)
extractInlineWriting =
  htmls (inlineWritingContainer // anySelector `atDepth` 1)
    <&> wrapUp . drop 8  -- file info header
  where
    inlineWritingContainer :: Selector
    inlineWritingContainer = "div" @. "submission-writing" // "center" // "div"

    wrapUp :: [HTML] -> Maybe HTML
    wrapUp paragraphs | length paragraphs > 1 = Just (T.concat paragraphs)
    wrapUp _ = Nothing  -- no preview, unsupported file type

extractSubmission :: URI -> Scraper T.Text Submission
extractSubmission page = do
  Just download <- attr "href" ("div" @. "download" // "a") <&> canonUri
  title <- text $ metaContainer // "div" @. "submission-title" // "p"
  author <- text $ metaContainer // "a" // "strong"
  date <- attr "title" $ metaContainer // "span" @. "popup_date"
  description <- html $ "div" @. "submission-description"
  tags <- extractTags
  folders <- extractFolderEntries page
  inlineWriting <- extractInlineWriting
  return Submission { .. }
  where
    metaContainer :: Selector
    metaContainer = "div" @. "submission-id-sub-container"

    canonUri :: T.Text -> Maybe URI
    canonUri = canonicaliseUri page

scrapeSubmission :: HTTP.Manager -> URI -> IO (Maybe Submission)
scrapeSubmission client uri = fetchAndScrape client (extractSubmission uri) uri