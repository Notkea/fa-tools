-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module List where

import qualified Conduit as C
import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Csv as CSV
import qualified Fa.Folder as FAF
import qualified Fa.Listing as FAL

import Conduit ((.|))

type URL = String

encodeCsv ::
  (CSV.DefaultOrdered a, CSV.ToNamedRecord a) => a -> LB.ByteString
encodeCsv a = CSV.encodeDefaultOrderedByNameWith noHeader [a]
  where
    noHeader :: CSV.EncodeOptions
    noHeader = CSV.defaultEncodeOptions {
      CSV.encIncludeHeader = False
    }

-- TODO: rate-limit when scrapeing many pages?
list :: HTTP.Manager -> URL -> Bool -> IO ()
list client mainUrl allFolders = do
  let Just uri = URI.parseURI mainUrl
  LB.putStr headers
  C.runConduit $
    FAL.scrapeListingPages client uri
    .| (if allFolders then scrapeOtherFolders else C.mapC id)
    .| C.concatMapC FAL.submissions
    .| C.mapM_C (LB.putStr . encodeCsv)

  where
    headers :: LB.ByteString
    headers = CSV.encodeDefaultOrderedByName ([] :: [FAL.SubmissionEntry])

    scrapeOtherFolders ::
      C.ConduitT FAL.ListingPageData FAL.ListingPageData IO ()
    scrapeOtherFolders = do
      Just firstPageData <- C.headC
      C.yield firstPageData
      C.mapC id
      C.toProducer $ scrapeFolders $ FAL.folders firstPageData

    scrapeFolders ::
      [FAF.FolderEntry] -> C.ConduitT () FAL.ListingPageData IO ()
    scrapeFolders [] = mempty
    scrapeFolders (FAF.FolderEntry { FAF.url } : others) = do
      FAL.scrapeListingPages client url
      scrapeFolders others
