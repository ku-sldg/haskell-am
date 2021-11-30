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
--import ExecCopland
import ServerAppUtil (startServer)

import Control.Concurrent

main :: IO ()
main = do
  putStrLn "Starting Connection Server..."
  startServer COMM doAt

doAt msg = do
  putStrLn $ "Beginning of doAt for ConnectionServerMain"
  putStrLn $ "Raw Message received: " ++ (show msg)
  req@(RequestMessage pTo pFrom names t e) <- decodeGen msg --decodeRequest msg
  putStrLn $ "received RequestMessage: " ++ (show req)
  tcpSock <- getTheirSock pTo names
  sendAll tcpSock msg
  putStrLn $ "forwarding RequestMessage: " ++ (show msg)
  
  respMsg <- recv tcpSock 2048

  -- TODO: decode here?  Or only on client side (since all we do is forward message)?
  let (val :: Maybe ResponseMessage) = DA.decodeStrict respMsg
  case val of
   Nothing -> error $ "weird message received: " ++ (show respMsg)
   Just res -> do
     putStrLn $ "decoded ResonseMessage: " ++ (show res)
     return respMsg

getTheirSock :: Plc -> M.Map Plc Address -> IO Socket
getTheirSock pThem nameServer = do
  let mString = M.lookup pThem nameServer
  case mString of
    Nothing -> error "my client error:  server port is not initialized in environment nameserver"
    Just portString -> do
                 addr <- clientResolve "127.0.0.1" portString
                 clientOpen addr
