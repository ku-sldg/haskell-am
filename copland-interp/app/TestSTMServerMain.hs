{-  Executable that activates the StoreServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Adam Petz (Tweaked earlier code by Ed Komp)
  Date:  12/16/2020
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString, empty)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Numeric.Natural
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)


import Copland
import CommUtil
import UDcore
import ExecCopland
import ServerAppUtil (startServer)
import MonadTestSTM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

main :: IO ()
main = do
  v <- atomically $ newTMVar 0
  let env = Test_Env v 


  
  startServer STORE (doAt env)

{-
store_loop :: StoreM ()
store_loop = do
  msg' <-
-}

inc_num :: TMVar Natural -> STM Natural
inc_num v = do
  val <- takeTMVar v
  putTMVar v (val + 1)
  return val

get_num :: TMVar Natural -> STM Natural
get_num v = readTMVar v 
  

test_handle_req :: TestRequestMessage -> TestM Natural
test_handle_req m = do
  var <- asks env_num
  case m of
    TestIncMessage -> do
      n <- liftIO $ atomically $ (inc_num var)
      liftIO $ putStrLn $ "Performed Inc on: " ++ (show n) ++ "\n"
      return n
    TestGetMessage -> do
      n <- liftIO $ atomically $ (get_num var)
      liftIO $ putStrLn $ "Performed Get on: " ++ (show n) ++ "\n"
      return n
  

doAt env msg = do
  msg' <- decodeGen msg --decodeRequest msg
  n <- run_testm (test_handle_req msg') env
  let respMsg = TestResponseMessage n
  let respBits = DA.encode respMsg
  threadDelay 1000000
  return (BL.toStrict respBits)




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
