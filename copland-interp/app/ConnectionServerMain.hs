{-  Executable that activates the ConnectionServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Ed Komp
  Date:  12/4/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Copland
import CommUtil
import UDcore
import ExecCopland
import ServerAppUtil (startServer, ServerType(COMM))

import Control.Concurrent

main :: IO ()
main = do
  startServer COMM doAt

doAt msg = do
  (RequestMessage pTo pFrom names t e) <- decodeGen msg --decodeRequest msg
  tcpSock <- getTheirSock pTo names
  sendAll tcpSock msg
  respMsg <- recv tcpSock 1024

  -- TODO: decode here?  Or only on client side (since all we do is forward message)?
  let (val :: Maybe ResponseMessage) = DA.decodeStrict respMsg
  case val of
   Nothing -> error $ "weird message received: " ++ (show respMsg)
   Just res ->
     return respMsg

getTheirSock :: Pl -> M.Map Pl Address -> IO Socket
getTheirSock pThem nameServer = do
  let mString = M.lookup pThem nameServer
  case mString of
    Nothing -> error "my client error:  server port is not initialized in environment nameserver"
    Just portString -> do
                 addr <- clientResolve "127.0.0.1" portString
                 clientOpen addr
