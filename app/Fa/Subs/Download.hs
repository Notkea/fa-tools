-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Subs.Download where

import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified System.IO.Error as IOE
import qualified Fa.Submission as FAS

import Control.Monad (when)
import Fa.Client
import Fa.Uri

type URL = String

download :: HTTP.Manager -> URL -> Maybe FilePath -> IO ()
download client url output = do
  let Just uri = URI.parseURI url
  fileUri <- getFileUri uri
  let outputPath = getOutputPath fileUri
  downloadStream client fileUri $ sinkFor outputPath
  when (outputPath /= "-") $ putStrLn outputPath

  where
    getFileUri :: URI.URI -> IO URI.URI
    getFileUri uri = case uriHostName uri of
      "furaffinity.net" -> getSubmissionUri uri
      "www.furaffinity.net" -> getSubmissionUri uri
      "d.furaffinity.net" -> return uri
      "t.furaffinity.net" -> return uri
      _ -> IOE.ioError $ IOE.userError "not a known FurAffinity domain"

    getSubmissionUri :: URI.URI -> IO URI.URI
    getSubmissionUri uri = do
      Just info <- FAS.scrapeSubmission client uri
      return $ FAS.download info

    getOutputPath :: URI.URI -> FilePath
    getOutputPath uri = case output of
      Just givenName -> givenName
      Nothing -> uriFileName uri
