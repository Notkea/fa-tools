-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Subs.Info where

import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Fa.Submissions.Submission as FAS

type URL = String

info :: HTTP.Manager -> URL -> IO ()
info client url = do
  let Just uri = URI.parseURI url
  Just submissionInfo <- FAS.scrapeSubmission client uri
  LB.putStrLn $ JSON.encode submissionInfo
