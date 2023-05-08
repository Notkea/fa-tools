-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module List where

import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Csv as CSV
import qualified Fa.Folder as FAF
import qualified Fa.Listing as FAL

import Control.Arrow ((>>>))

type URL = String

-- TODO: rate-limit when scrapeing many pages?
-- TODO: stream output?
-- TODO: deduplicate output?
list :: HTTP.Manager -> URL -> Bool -> IO ()
list client url allFolders = do
  let Just uri = URI.parseURI url
  mainFolderPages <- scrapeFolder uri
  otherFoldersPages <- scrapeOtherFolders $ FAL.folders $ head mainFolderPages
  printSubmissionsCsv $ mainFolderPages ++ otherFoldersPages

  where
    scrapeFolder :: URI.URI -> IO [FAL.ListingPageData]
    scrapeFolder = FAL.scrapeListingDataMultiPage client

    scrapeOtherFolders :: [FAF.FolderEntry] -> IO [FAL.ListingPageData]
    scrapeOtherFolders folderEntries
      | allFolders = concat <$> mapM (scrapeFolder . FAF.url) folderEntries
      | otherwise = return []

    printSubmissionsCsv :: [FAL.ListingPageData] -> IO ()
    printSubmissionsCsv =
      concatMap FAL.submissions
      >>> CSV.encodeDefaultOrderedByName
      >>> LB.putStrLn
