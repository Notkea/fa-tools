-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Common where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Fa.Client as FAC

import System.Environment (getEnv)

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
