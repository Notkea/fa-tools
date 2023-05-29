-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.Listing where

import qualified Conduit as C
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Csv as CSV
import qualified Network.URI.Static as U

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Network.URI (URI)
import Data.Functor ((<&>))
import Fa.Client ((@.), hasMatch, link, fetchAndScrapePages)
import Fa.Uri (canonicaliseUri)
import Fa.Date (extractAbsDate)

instance CSV.ToField Bool where
  toField True = "true"
  toField False = "false"

data NoteEntry = NoteEntry
  -- Notes don't have a fixed URI (they depend on the current page).
  -- Use their numeric identifier instead.
  { identifier :: Int
  , sender :: URI
  , date :: T.Text
  , unread :: Bool
  , subject :: T.Text
  } deriving (Generic, Show, CSV.ToNamedRecord, CSV.DefaultOrdered)

data NoteListPageData = NoteListPageData
  { notes :: [NoteEntry]
  , nextPage :: Maybe URI
  } deriving (Show)

extractNoteEntries :: URI -> Scraper T.Text [NoteEntry]
extractNoteEntries baseUri =
  chroots ("div" @. "message-center-pms-note-list-view") $ do
    Right (identifier, _) <- attr "value" "input" <&> T.decimal
    Just sender <- link baseUri "href" ("div" @. "note-list-sender" // "a")
    Just date <- extractAbsDate ("div" @. "note-list-senddate")
    unread <- hasMatch ("a" @. "note-unread")
    subject <- text ("div" @. "note-list-subject") <&> T.strip
    return NoteEntry { .. }

extractNextPageUrl :: URI -> Scraper T.Text (Maybe URI)
extractNextPageUrl baseUri =
  fmap (lookup "Older") $
    chroots ("div" @. "messagecenter-mail-list" // "a" @. "button") $ do
      label <- text "a"
      Just target <- attr "href" "a" <&> otherPageUri
      return (label, target)
  where
    otherPageUri :: T.Text -> Maybe URI
    otherPageUri "#" = Nothing
    otherPageUri other = canonicaliseUri baseUri other

extractNoteListPageData :: URI -> Scraper T.Text NoteListPageData
extractNoteListPageData baseUri = do
  notes <- extractNoteEntries baseUri
  nextPage <- extractNextPageUrl baseUri
  return NoteListPageData { .. }

-- TODO: handle the folders other than the default one
--       (inbox, sent, archive, trash, ...).
--       This is set through a cookie for some reason...
--       (i.e.: "sessionstuff; folder=inbox").
scrapeNoteListPages :: HTTP.Manager -> C.ConduitT () NoteListPageData IO ()
scrapeNoteListPages client =
  fetchAndScrapePages client extractNoteListPageData nextPage noteListUri
  where
    noteListUri :: URI
    noteListUri = $$(U.staticURI "https://www.furaffinity.net/msg/pms/")
