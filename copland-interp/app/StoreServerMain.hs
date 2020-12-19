{-  Executable that activates the StoreServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Adam Petz (Tweaked earlier code by Ed Komp)
  Date:  12/16/2020
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Numeric.Natural


import Copland
import CommUtil
import UDcore
import ExecCopland
import ServerAppUtil (startServer)
import MonadStore

import Control.Concurrent.STM
import qualified Data.Set as S 


initStoreEnv :: IO Store_Env
initStoreEnv = atomically $ do
  imap <- newTMVar M.empty
  smap <- newTMVar M.empty
  indexes <- newTMVar S.empty
  return (Store_Env imap smap indexes)
  

main :: IO ()
main = do
  env <- initStoreEnv
  startServer STORE (doAt env)

{-
store_loop :: StoreM ()
store_loop = do
  msg' <-
-}

doAt env msg = do
  msg' <- decodeGen msg --decodeRequest msg
  case msg' of
    SetMessage m -> error "hi"
    GetMessage m -> error "hey"




    {-
    --(StoreRequestMessage i e) -> error "HHH"
    (RequestMessage pTo pFrom names t e) -> do
      tcpSock <- getTheirSock pTo names
      sendAll tcpSock msg
      respMsg <- recv tcpSock 1024

      -- TODO: decode here?  Or only on client side (since all we do is forward message)?
      let (val :: Maybe ResponseMessage) = DA.decodeStrict respMsg
      case val of
        Nothing -> error $ "weird message received: " ++ (show respMsg)
        Just res -> return respMsg
-}
    

    

    --_ -> error "Unexpected msg type in StoreServerMain doAt()"

getTheirSock :: Pl -> M.Map Pl Address -> IO Socket
getTheirSock pThem nameServer = do
  let mString = M.lookup pThem nameServer
  case mString of
    Nothing -> error "my client error:  server port is not initialized in environment nameserver"
    Just portString -> do
                 addr <- clientResolve "127.0.0.1" portString
                 clientOpen addr
