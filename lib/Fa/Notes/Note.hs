-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.Note where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Text.HTML.Scalpel as S

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Time (ZonedTime)
import Network.URI (URI, parseURI)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Fa.Extractors ((@.), (@#), links, fetchAndScrape)
import Fa.Date (extractAbsDate)
import Fa.Uri (uriString)

type NoteID = Int

data Note = Note
  { identifier :: NoteID
  , sender :: URI
  , recipient :: URI
  , date :: ZonedTime
  , subject :: T.Text
  , content :: T.Text
  } deriving (Generic, Show, ToJSON)

extractNote :: NoteID -> URI -> Scraper T.Text Note
extractNote identifier page =
  chroot ("div" @. "messagecenter-mail-note-preview-pane") $ do
    [Just sender, Just recipient] <- links page "href" (msgHeader // "a")
    Just date <- extractAbsDate msgHeader
    subject <- text ("div" @. "section-header" // "h2")

    -- use content from the reply field to retrieve plain text instead of html
    -- if available, otherwise use the html (for deleted correspondants)
    content <- (text ("textarea" @# "JSMessage_reply") <&> stripTextReply)
           <|> (text ("div" @. "user-submitted-links") <&> T.strip)

    return Note { .. }
  where
    msgHeader :: Selector
    msgHeader = "div" @. "addresses"

    stripTextReply :: T.Text -> T.Text
    stripTextReply = T.unlines . map T.strip . drop 6 . T.lines

noteUri :: NoteID -> Maybe URI
noteUri noteId =
  -- using an arbitrary listing page index (choice doesn't matter)
  parseURI $ "https://www.furaffinity.net/msg/pms/1/" ++ show noteId

scrapeNote :: HTTP.Manager -> NoteID -> IO (Maybe Note)
scrapeNote client noteId = do
  let Just uri = noteUri noteId
  fetchAndScrape client (extractNote noteId uri) uri

data NoteTargetFolder = Unread | Archive | Trash
  deriving (Show, Read, Typeable, Data)

moveNote :: HTTP.Manager -> NoteID -> NoteTargetFolder -> IO ()
moveNote client noteId targetFolder = do
  let Just uri = noteUri noteId
  req <- HTTP.parseRequest (uriString uri) <&> HTTP.urlEncodedBody formData
  _ <- HTTP.httpNoBody req client
  return ()
  where
    formData :: [(ByteString, ByteString)]
    formData =
      [ ("manage_notes", "1")
      , ("items[]", BSC.pack $ show noteId)
      , ("move_to", BSC.pack $ map toLower $ show targetFolder)
      ]
