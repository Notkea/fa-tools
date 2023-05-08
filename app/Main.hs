-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Fa.Client as FAC

import qualified Download as APP
import qualified List as APP
import qualified Info as APP

import System.Environment (getEnv)
import System.Console.CmdArgs

envKeyFaSessionHeaders :: String
envKeyFaSessionHeaders = "FA_SESSION_HEADERS"

envHelp :: [String]
envHelp = map (\(key, text) -> "$" ++ key ++ ": " ++ text)
  [ (envKeyFaSessionHeaders, "headers of an existing session")
  ]

initHttpManager :: IO HTTP.Manager
initHttpManager = do
  headerLines <- getEnv envKeyFaSessionHeaders
  let headers = FAC.parseHeaderLines $ T.pack headerLines
  FAC.newHttpManagerWithSession $ FAC.filterSessionHeaders headers

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
run client Download { url, output } = APP.download client url output
run client List { url, allFolders } = APP.list client url allFolders
run client Info { url } = APP.info client url
