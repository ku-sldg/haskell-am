{-  Executable that activates the ConnectionServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Ed Komp
  Date:  12/4/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Copland
import CommUtil
import UDcore
import ExecCopland

import Control.Concurrent

-- socketPathname is currently a global constant in the ConnectionServer module
-- this must change, soon ....
main :: IO ()
main = do
  socketPathname <- lookupUDsocketPath
  runUnixDomainServer socketPathname doAt


doAt udSocket = do
          msg <- recv udSocket 1024
          (RequestMessage pTo pFrom names t e) <- decodeRequest msg
          tcpSock <- getTheirSock pTo names
          sendAll tcpSock msg
          respMsg <- recv tcpSock 1024
          let (val :: Maybe ResponseMessage) = DA.decodeStrict respMsg
          case val of
              Nothing -> error $ "weird message received: " ++ (show respMsg)
              Just res -> do
                     sendAll udSocket respMsg


{- Confirm the input is in valid form, and return the RequestMessage -}
decodeRequest :: BS -> IO RequestMessage
decodeRequest msg = do
          let (val :: Maybe RequestMessage) = DA.decodeStrict msg
          case val of
            Nothing -> error $ "weird message received: " ++ (show msg)
            Just res -> return res


getTheirSock :: Pl -> M.Map Pl Address -> IO Socket
getTheirSock pThem nameServer = do
  let mString = M.lookup pThem nameServer
  case mString of
    Nothing -> error "my client error:  server port is not initialized in environment nameserver"
    Just portString -> do
                 addr <- clientResolve "127.0.0.1" portString
                 clientOpen addr
