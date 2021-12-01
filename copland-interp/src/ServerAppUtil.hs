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
--import Interp (interp)
import MonadCop (runCOP, buildServerEnv)
--import MonadVM_Old
--import ExecCopland (run_vm)
--import qualified DemoStates as DS (vm_state_init)
import Impl_VM_Extracted (run_cvm_rawev)
import StVM (Coq_cvm_st(..))
import DemoStates
import StVM_Deriving

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

{-
fromRemote :: NS.Socket -> SA.Server_Options -> IO ()
fromRemote conn opts = do
  rreq@(RequestMessage pTo pFrom names t e) <- receiveReq conn
  --error $ (show names)

  putStrLn $ "Req received: " ++ (show rreq) ++ "\n"

  --(reqs,store) <- undefined --derive_comm_reqs (annotated t) names pTo -- TODO: Check pTo here
  --setupComm reqs

  store <- undefined
  
  env <- buildServerEnv opts names pTo store undefined

  

{-
  let compileB = SA.server_compile opts
  e' <- case compileB of
         True -> do
           {-let instrs = instr_compiler t -}
           vm_st <- DS.vm_state_init e
           putStrLn $ "HHHEEERE"
           res <- undefined --run_vm (annotated t) vm_st env
           return $ undefined --st_ev res
         False -> do
           putStrLn $ "HERE"
           undefined--runCOP (interp t e) env
-}

  e' <- undefined
  
  --e' <- run_interp t e env
  --putStrLn $ "evidence gathered: " ++ (show e')
  sendResp conn pTo pFrom e'
  NS.close conn

-}

get_my_pl :: SA.Server_Options -> Plc
get_my_pl opts =
  case (SA.server_serverType opts) of
    CVM_SERV params -> cvm_params_plc params
    _ -> error "Expected CVM_SERV server type, got something else..."

fromRemote :: SA.Server_Options -> {-BS.ByteString ->-} NS.Socket -> IO ()
fromRemote opts conn = do
  rreq@(RequestMessage pTo pFrom names t e) <- receiveReq conn

  print "received RequestMessage: "
  print rreq
  let store = M.empty
  let me = get_my_pl opts
  env <- buildServerEnv opts names me store sample_aspmap
  let st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0)

  print "init state: "
  print st 
  res_rawev <- run_cvm_rawev t st env

  sendResp conn pFrom me res_rawev



{-
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
-}


{-
serve_requests :: NS.Socket -> SA.Server_Options -> IO ()
serve_requests sock opts = forever $ do
  putStrLn $ "BEFORE CONNECTION"
  (conn, peer) <- NS.accept sock
  putStrLn $ "Connection from " ++ show peer
  --error (show opts)
  let server_type = SA.server_serverType opts
  case server_type of
    CVM_SERV _ -> 
      void $ CC.forkIO $ fromRemote conn opts
    _ -> return ()
-}


{-
spawn_a_server :: Bool -> Bool -> Address -> IO ()
spawn_a_server sim debug addr = do
  let sopts = SA.Server_Options sim debug addr undefined
  void $ CC.forkIO $ start_standalone_server sopts
-}

  {-
spawn_the_servers :: M.Map Plc Address -> Bool -> Bool -> IO ()
spawn_the_servers nm simB debugB = do
  let ps = M.toList nm
      snds = map snd ps
  mapM_ (spawn_a_server simB debugB) snds
-}


start_server :: SA.Server_Options -> IO ()
start_server opts = do
  let addr = SA.server_serverPort opts
      servType = SA.server_serverType opts
  case (servType) of
    CVM_SERV params -> start_server' addr (fromRemote opts)
    _ -> return ()
    


start_server' :: Address -> (NS.Socket -> IO a) -> IO a
start_server' path f =
  runUnixDomainServer path f




{-

startServer :: ServerType -> (BS.ByteString -> IO BS.ByteString) -> IO ()
startServer x f = do
  --error "here"
  socketPathname <- lookupPath x
  putStrLn socketPathname
  runUnixDomainServer socketPathname (handleSockBits f)

handleSockBits :: (BS.ByteString -> IO BS.ByteString) -> NS.Socket -> IO ()
handleSockBits f s = do
  --putStrLn "in handleSockBits"
  --error "handle before recv"
  msg <- recv s 2048
  --putStrLn $ "msg: " ++ (show msg)
  --error "handle after recv"
  resp <- f msg
  sendAll s resp
-}
