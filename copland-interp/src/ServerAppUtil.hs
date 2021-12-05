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
import Comm
import UDcore
import qualified ServerProgArgs as SA (Server_Options(..))
import MonadCop (runCOP)
import Impl_VM_Extracted (run_cvm_rawev)
import StVM (Coq_cvm_st(..))
import DemoStates
import StVM_Deriving
import BS
import CryptoImpl (doSign)
import ServerAppHandlers

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B (readFile)
import qualified Network.Socket as NS hiding  (recv, sendAll)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode, ToJSON, FromJSON)
import qualified Control.Concurrent as CC (forkIO, threadDelay)
import Control.Monad (forever, void)


start_server :: SA.Server_Options -> IO ()
start_server opts = do
  let addr = SA.server_serverPort opts
      servType = SA.server_serverType opts
  case (servType) of
    CVM_SERV params -> do
      let simB = SA.server_optSim opts
          debugB = SA.server_optDebug opts
      start_server'' addr
                     (handle_remote params simB debugB)
    SIGN ->
      start_server'' addr
                     (handle_sig)
    ASP_SERV _ ->
      start_server'' addr
                     (handle_asp)
    _ -> return ()


start_server'' :: (DA.ToJSON a, DA.FromJSON a, DA.ToJSON b, DA.FromJSON b) =>
                  Address -> (a -> IO b) -> IO ()
start_server'' addr f = do
  runUnixDomainServer addr (gen_server_session f)
















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
