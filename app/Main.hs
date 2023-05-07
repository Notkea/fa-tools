-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as HTTPC
import qualified Data.Text as T
import qualified Conduit as C
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Csv as CSV

import Conduit ((.|))
import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import System.Environment (getEnv)
import System.FilePath.Posix (takeBaseName)
import Control.Monad.Trans.Resource (runResourceT)

import System.Console.CmdArgs
import Fa.Client
import qualified Fa.Listing as FAL

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
  deriving (Show, Data, Typeable)

optionsModes :: Options
optionsModes = modes
  [ Download
      { url = def
          &= typ "FILE_URL"
          &= argPos 0
      , output = Nothing
          &= typFile
          &= help "Output file (default: name from URL)"
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
  request <- HTTP.parseRequest url
  runResourceT $ do
    response <- HTTPC.http request client
    C.runConduit $ HTTPC.responseBody response .| sink output
  where
    sink :: C.MonadResource m => Maybe FilePath -> C.ConduitT ByteString o m ()
    sink (Just "-") = C.stdoutC
    sink (Just path) = C.sinkFile path
    sink Nothing = C.sinkFile $ takeBaseName url

-- TODO: proper exit code (page not existing, etc)
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

    scrapeOtherFolders :: [FAL.FolderEntry] -> IO [FAL.ListingPageData]
    scrapeOtherFolders folderEntries | allFolders =
      concat <$> mapM (scrapeFolder . FAL.url) folderEntries
    scrapeOtherFolders _ = return []

    printSubmissionsCsv :: [FAL.ListingPageData] -> IO ()
    printSubmissionsCsv =
      concatMap FAL.submissions
      >>> CSV.encodeDefaultOrderedByName
      >>> LB.putStrLn
