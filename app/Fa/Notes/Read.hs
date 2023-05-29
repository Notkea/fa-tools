-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.Read where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Fa.Note as FAN

type URL = String

read :: HTTP.Manager -> Int -> IO ()
read client identifier = do
  Just note <- FAN.scrapeNote client identifier
  LB.putStrLn $ JSON.encode note
