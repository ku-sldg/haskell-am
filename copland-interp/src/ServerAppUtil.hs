{-  Executable that activates the ConnectionServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Ed Komp
  Date:  12/4/2019

  Adapted by: Adam Petz
  Date:  4/4/2020
-}

{-# LANGUAGE ScopedTypeVariables #-}

module ServerAppUtil where

import Copland
import CommUtil
import UDcore

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import System.Environment (lookupEnv)
import Control.Concurrent

startServer :: ServerType -> (BS.ByteString -> IO BS.ByteString) -> IO ()
startServer x f = do
  --error "here"
  socketPathname <- lookupPath x
  --error socketPathname
  runUnixDomainServer socketPathname (handleSockBits f)

handleSockBits :: (BS.ByteString -> IO BS.ByteString) -> Socket -> IO ()
handleSockBits f s = do
  error "handle before recv"
  msg <- recv s 1024
  error "handle after recv"
  resp <- f msg
  sendAll s resp

data ServerType =
  COMM
  | SIGN
  | ASP_SERV ASP_ID

-- socketPathname is currently a global constant here
-- this must change, soon ....
lookupPath :: ServerType -> IO FilePath
lookupPath v = do
  let tag =
        case v of
        COMM -> "COMM"
        SIGN -> "SIG"
        ASP_SERV i -> "ASP_" ++ (show i)
  let custom_path = "COPLAND_" ++ tag ++ "_SOCKET"
  maybeBuildPath <- lookupEnv "COPLAND_BUILD" -- TODO: fix hardcoding
  maybeSocketPath  <- lookupEnv $ custom_path
  socketPath <-
        case maybeSocketPath of
        Just p -> return p
        Nothing ->
          case maybeBuildPath of
           Just s -> do
             return $ s ++ "/" ++ tag
           Nothing ->
             error $ "Missing both COPLAND_BUILD(for default path) and " ++ custom_path ++ "(for custom path) environment variables.  Must have one or the other to connect to the " ++ tag ++ "Server."
  return socketPath
