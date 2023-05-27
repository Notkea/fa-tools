-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Date where

import qualified Data.Text as T

import Text.HTML.Scalpel as S

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Fa.Client ((@.))

type Date = T.Text

extractAbsDate :: Selector -> Scraper T.Text (Maybe Date)
extractAbsDate sel = do
  dateFromText <- text dateSelector <&> filterDate
  dateFromTitle <- attr "title" dateSelector <&> filterDate
  return (dateFromText <|> dateFromTitle)
  where
    dateSelector :: Selector
    dateSelector = sel // "span" @. "popup_date"

    filterDate :: T.Text -> Maybe Date
    filterDate t | ":" `T.isInfixOf` t = Just t
    filterDate _ = Nothing  -- ignore fuzzy date
