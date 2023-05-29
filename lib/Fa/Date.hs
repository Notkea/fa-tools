-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Date where

import qualified Data.Text as T
import qualified Text.Regex.TDFA as RE
import qualified Data.Csv as CSV

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Fa.Extractors ((@.))
import Data.Time
import Data.Time.Format.ISO8601
import Text.HTML.Scalpel

instance CSV.ToField ZonedTime where
  toField = CSV.toField . iso8601Show

-- FA date formatting isn't consistent across the site:
-- * "Oct 2, 2016 09:39 AM" (submission)
-- * "Oct 2, 2016 09:39AM" (note list)
-- * "Oct 2nd, 2016, 09:39 AM" (note body)
parseDate :: T.Text -> Maybe LocalTime
parseDate =
  extract "([a-zA-Z]+) ([0-9]+)[a-z,]+ ([0-9]+),? ([0-9]+):([0-9]+) ?([A-Z]+)"
  >>> parse "%b %-d %Y %l %M %p" . T.unwords
  where
    extract :: T.Text -> T.Text -> [T.Text]
    extract re input = tail $ RE.getAllTextSubmatches (input RE.=~ re)

    parse :: String -> T.Text -> Maybe LocalTime
    parse format = parseTimeM True defaultTimeLocale format . T.unpack

extractAbsDate :: Selector -> Scraper T.Text (Maybe ZonedTime)
extractAbsDate sel = do
  dateFromText <- text dateSelector <&> filterDate
  dateFromTitle <- attr "title" dateSelector <&> filterDate
  let dateField = dateFromText <|> dateFromTitle
  return $ dateField >>= parseDate <&> withServerTimeZone
  where
    dateSelector :: Selector
    dateSelector = sel // "span" @. "popup_date"

    filterDate :: T.Text -> Maybe T.Text
    filterDate t | ":" `T.isInfixOf` t = Just t
    filterDate _ = Nothing  -- ignore fuzzy date

    -- The server time seems to match the PDT time zone.
    withServerTimeZone :: LocalTime -> ZonedTime
    withServerTimeZone = flip ZonedTime $ read "PDT"
