-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Csv as CSV
import qualified Data.Aeson as JSON
import qualified System.IO.Error as IOE

import Control.Arrow ((>>>))
import System.Environment (getEnv)

import System.Console.CmdArgs
import Fa.Client
import Fa.Uri
import qualified Fa.Folder as FAF
import qualified Fa.Listing as FAL
import qualified Fa.Submission as FAS

envKeyFaSessionHeaders :: String
envKeyFaSessionHeaders = "FA_SESSION_HEADERS"

envHelp :: [String]
envHelp = map (\(key, text) -> "$" ++ key ++ ": " ++ text)
  [ (envKeyFaSessionHeaders, "headers of an existing session")
  ]

initHttpManager :: IO HTTP.Manager
initHttpManager = do
  headerLines <- getEnv envKeyFaSessionHeaders
  let headers = parseHeaderLines $ T.pack headerLines
  newHttpManagerWithSession $ filterSessionHeaders headers

type URL = String

data Options
  = Download
      { url :: URL
      , output :: Maybe FilePath
      }
  | List
      { url :: URL
      , allFolders :: Bool
      }
  | Info
      { url :: URL
      }
  deriving (Show, Data, Typeable)

optionsModes :: Options
optionsModes = modes
  [ Download
      { url = def
          &= typ "SUBMISSION_URL"
          &= argPos 0
      , output = Nothing
          &= typFile
          &= help "Output file (default: original name)"
      }
  , List
      { url = def
          &= typ "LIST_PAGE_URL"
          &= argPos 0
      , allFolders = False
          &= explicit
          &= name "a"
          &= name "all-folders"
          &= help "List items from all folders (default: false)"
      }
  , Info
      { url = def
          &= typ "SUBMISSION_PAGE_URL"
          &= argPos 0
      }
  ]
  &= summary "A CLI toolbox to download content from FurAffinity."
  &= program "fa-tools"
  &= help (unlines envHelp)
  &= helpArg [explicit, name "h", name "help"]
  &= versionArg [explicit, name "version"]

main :: IO ()
main = do
  arguments <- cmdArgs optionsModes
  client <- initHttpManager
  run client arguments

run :: HTTP.Manager -> Options -> IO ()

run client Download { url, output } = do
  let Just uri = URI.parseURI url
  fileUri <- getFileUri uri
  let outputPath = getOutputPath fileUri
  downloadStream client fileUri $ sinkFor outputPath
  putStrLn outputPath
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

-- TODO: proper exit code (page not existing, etc)
-- TODO: rate-limit when scrapeing many pages?
-- TODO: stream output?
-- TODO: deduplicate output?
run client List { url, allFolders } = do
  let Just uri = URI.parseURI url
  mainFolderPages <- scrapeFolder uri
  otherFoldersPages <- scrapeOtherFolders $ FAL.folders $ head mainFolderPages
  printSubmissionsCsv $ mainFolderPages ++ otherFoldersPages

  where
    scrapeFolder :: URI.URI -> IO [FAL.ListingPageData]
    scrapeFolder = FAL.scrapeListingDataMultiPage client

    scrapeOtherFolders :: [FAF.FolderEntry] -> IO [FAL.ListingPageData]
    scrapeOtherFolders folderEntries | allFolders =
      concat <$> mapM (scrapeFolder . FAF.url) folderEntries
    scrapeOtherFolders _ = return []

    printSubmissionsCsv :: [FAL.ListingPageData] -> IO ()
    printSubmissionsCsv =
      concatMap FAL.submissions
      >>> CSV.encodeDefaultOrderedByName
      >>> LB.putStrLn

run client Info { url } = do
  let Just uri = URI.parseURI url
  Just info <- FAS.scrapeSubmission client uri
  LB.putStrLn $ JSON.encode info
