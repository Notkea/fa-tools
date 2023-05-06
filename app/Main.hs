-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as HTTPC
import qualified Data.Text as T
import qualified Conduit as C

import Conduit ((.|))
import Data.ByteString (ByteString)
import System.Environment (getEnv)
import System.FilePath.Posix (takeBaseName)
import Control.Monad.Trans.Resource (runResourceT)

import System.Console.CmdArgs
import Fa.Client

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
  | Dummy
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
  , Dummy
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

run _ Dummy = return ()
