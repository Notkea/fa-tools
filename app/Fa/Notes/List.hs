-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.List where

import qualified Conduit as C
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Csv as CSV
import qualified Fa.Notes.Listing as FANL

import Conduit ((.|))

type URL = String

-- TODO: factorise that
encodeCsv ::
  (CSV.DefaultOrdered a, CSV.ToNamedRecord a) => a -> LB.ByteString
encodeCsv a = CSV.encodeDefaultOrderedByNameWith noHeader [a]
  where
    noHeader :: CSV.EncodeOptions
    noHeader = CSV.defaultEncodeOptions {
      CSV.encIncludeHeader = False
    }

list :: HTTP.Manager -> IO ()
list client = do
  LB.putStr headers
  C.runConduit $
    FANL.scrapeNoteListPages client
    .| C.concatMapC FANL.notes
    .| C.mapM_C (LB.putStr . encodeCsv)

  where
    headers :: LB.ByteString
    headers = CSV.encodeDefaultOrderedByName ([] :: [FANL.NoteEntry])
