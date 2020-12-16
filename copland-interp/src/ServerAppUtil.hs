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
--import CommUtil
import Comm
import UDcore
import qualified ServerProgArgs as SA (Server_Options(..))
import Interp (interp)
import MonadCop (runCOP, buildServerEnv)
import MonadVM
import ExecCopland (run_vm)
import qualified DemoStates as DS (vm_state_init)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString)
import qualified Network.Socket as NS hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
--import System.Environment (lookupEnv)
--import Control.Concurrent
import qualified Control.Concurrent as CC (forkIO, threadDelay)
import Control.Monad (forever, void)

{-  Acts as a server for handling remote attestation requests.
    Params:
      conn- connection handle (Socket) to the client
      opts- server config options provided as command-line arguments

TODO:  move this somewhere besides Interp.hs.
-}
fromRemote :: NS.Socket -> SA.Server_Options -> IO ()
fromRemote conn opts = do
  (RequestMessage pTo pFrom names t e) <- receiveReq conn
  --error $ (show names)
  env <- buildServerEnv opts names pTo

  let compileB = SA.server_compile opts
  e' <- case compileB of
         True -> do
           {-let instrs = instr_compiler t -}
           vm_st <- DS.vm_state_init e
           putStrLn $ "HHHEEERE"
           res <- run_vm (annotated t) vm_st env
           return $ st_ev res
         False -> do
           putStrLn $ "HERE"
           runCOP (interp t e) env
  
  --e' <- run_interp t e env
  --putStrLn $ "evidence gathered: " ++ (show e')
  sendResp conn pTo pFrom e'
  NS.close conn


start_standalone_server :: SA.Server_Options -> IO ()
start_standalone_server opts = do
  let pString = SA.server_serverPort opts
  portString <-
    case pString of
     "" -> newTCPPortString
     s -> return s
  
  addr <- resolve portString
  sock <- open addr
  putStrLn $ "starting server" ++ " at portttt: " ++ portString
  serve_requests sock opts {-void $ CC.forkIO $-} 

serve_requests :: NS.Socket -> SA.Server_Options -> IO ()
serve_requests sock opts = forever $ do
  putStrLn $ "BEFORE CONNECTION"
  (conn, peer) <- NS.accept sock
  putStrLn $ "Connection from " ++ show peer
  --error (show opts)
  void $ CC.forkIO $ fromRemote conn opts

spawn_a_server :: Bool -> Bool -> Bool -> Address -> IO ()
spawn_a_server sim debug compile addr = do
  let sopts = SA.Server_Options sim debug addr compile
  void $ CC.forkIO $ start_standalone_server sopts
  
spawn_the_servers :: M.Map Pl Address -> Bool -> Bool -> Bool -> IO ()
spawn_the_servers nm simB debugB compileB = do
  let ps = M.toList nm
      snds = map snd ps
  mapM_ (spawn_a_server simB debugB compileB) snds











startServer :: ServerType -> (BS.ByteString -> IO BS.ByteString) -> IO ()
startServer x f = do
  --error "here"
  socketPathname <- lookupPath x
  putStrLn socketPathname
  runUnixDomainServer socketPathname (handleSockBits f)

handleSockBits :: (BS.ByteString -> IO BS.ByteString) -> NS.Socket -> IO ()
handleSockBits f s = do
  putStrLn "in handleSockBits"
  --error "handle before recv"
  msg <- recv s 1024
  putStrLn $ "msg: " ++ (show msg)
  --error "handle after recv"
  resp <- f msg
  sendAll s resp
