{-  Executable that activates the ConnectionServer.
    Any attestation process requires that a (single) ConnectionServer
    already be active on the machine.

  Author: Ed Komp
  Date:  12/4/2019

  Adapted by: Adam Petz
  Date:  4/4/2020
-}

{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}

module ServerAppUtil where

import Copland
--import Comm
import CommUtil
import UDcore
import qualified ServerProgArgs as SA (Server_Options(..))
import MonadCop (runCOP)
import Impl_VM_Extracted (run_cvm_rawev)
import StVM (Coq_cvm_st(..))
--import DemoStates
import StVM_Deriving
import BS
import CryptoImpl (doSign)
import ServerAppHandlers
import GenServerOpts (get_server_addr_gen, get_places, get_asps_terms)
import qualified Example_Phrases_Admits as EPA (cache_id, attest_id, appraise_id, cert_id)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B (readFile)
import qualified Network.Socket as NS hiding  (recv, sendAll)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode, ToJSON, FromJSON)
import qualified Control.Concurrent as CC (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Concurrent.STM.TVar
import qualified Data.Aeson as DA (encode)

import Data.List(nub, concatMap)


gen_TCP_port_if_empty :: Address -> IO Address
gen_TCP_port_if_empty addr =
   case addr of
     "" -> newTCPPortString
     s -> return s
     
start_server :: SA.Server_Options -> IO ()
start_server opts = do
  let addr' = SA.server_serverPort opts
      servType = SA.server_serverType opts
  addr <- gen_TCP_port_if_empty addr'
     
  case (servType) of
    CVM_SERV params -> do
      {-
      let simB = SA.server_optSim opts
          debugB = SA.server_optDebug opts -}
      start_server'' addr
                     (handle_remote params opts)
    PAR_SERV params -> do
      do_par_server params opts addr
      
    SIGN ->
      start_server'' addr
                     (handle_sig opts)
    ASP_SERV i -> do
      putStrLn $ "Starting ASP server with ID: " ++ (show i)
      if | i == EPA.cache_id -> do
           cache_var <- newTVarIO Nothing
           start_server'' addr (handle_asp_cache cache_var)
         | i == EPA.attest_id -> do
             start_server'' addr (handle_asp_attest)
         | i == EPA.appraise_id -> do
             start_server'' addr (handle_asp_appraise)
         | i == EPA.cert_id -> do
             start_server'' addr (handle_asp_certify)
         | otherwise -> do
             start_server'' addr (handle_asp_default i)
    _ -> return ()


--   store_var <- newTVarIO M.empty

do_par_server' :: TVar (M.Map Loc RawEv) -> TVar [Loc] -> CVM_SERV_Params ->
                  SA.Server_Options -> NS.Socket -> IO ()
do_par_server' store_var locs_var params opts conn = do
  msg <- NBS.recv conn 2048
  (msg_decoded :: RequestMessagePar) <- decodeGen msg
  case msg_decoded of
    ParInit m -> do
      putStrLn $"received InitMessagePar: " ++ (show m)
      resp_msg <- handle_par_init locs_var m
      let msg'_encoded =  DA.encode resp_msg
      NBS.sendAll conn (BL.toStrict msg'_encoded)
      
    ParStart m -> handle_par_req params opts store_var m
    ParWait m -> do
      resp_msg <- handle_par_wait store_var locs_var m
      let msg'_encoded =  DA.encode resp_msg
      NBS.sendAll conn (BL.toStrict msg'_encoded)

do_par_server :: CVM_SERV_Params -> SA.Server_Options -> Address -> IO ()
do_par_server params opts addr = do
  store_var <- newTVarIO M.empty
  loc_var <- newTVarIO [1..100] -- TODO: pick more principled starting list?
  runUnixDomainServer addr (do_par_server' store_var loc_var params opts)

start_server'' :: (DA.ToJSON a, DA.FromJSON a, DA.ToJSON b, DA.FromJSON b) =>
                  Address -> (a -> IO b) -> IO ()
start_server'' addr f = do
  runUnixDomainServer addr (gen_server_session f)
  
spawn_server_thread :: SA.Server_Options -> IO ()
spawn_server_thread opts = void $ CC.forkIO $ start_server opts
  
spawn_the_server_threads :: [SA.Server_Options] -> IO ()
spawn_the_server_threads ls =
  mapM_ spawn_server_thread ls

spawn_servers_terms :: Bool -> Bool -> Bool -> Bool -> [Term] -> [Plc] -> IO ()
spawn_servers_terms simB debugB cvmSpawnB aspSpawnB ts ps_in = do
  let ps' = concatMap get_places ts
      ps = nub (ps_in ++ ps')

      cvm_opts =
        case (cvmSpawnB) of
          True -> gen_cvm_server_opts simB debugB aspSpawnB ps
          False -> []

      asp_opts =
        case (aspSpawnB) of
          True -> let as = nub (get_asps_terms ts ps) in
                    gen_asp_server_opts simB debugB as
          False -> []
        
      sig_opts = gen_sig_server_opts simB debugB ps
      par_opts = gen_par_server_opts simB debugB aspSpawnB ps

      opts = cvm_opts ++ asp_opts ++ sig_opts ++ par_opts
      
  --let opts = gen_term_opts simB debugB t p
  --print opts
  --error "DONE"
  spawn_the_server_threads opts
      

gen_server_opt :: Bool -> Bool -> ServerType -> Plc -> SA.Server_Options
gen_server_opt simB debugB stype p =
  let addr = get_server_addr_gen stype p in
    SA.Server_Options simB debugB addr stype


gen_sig_server_opts :: Bool -> Bool -> [Plc] -> [SA.Server_Options]
gen_sig_server_opts simB debugB ps =
  map (gen_server_opt simB debugB SIGN) ps

gen_asp_server_opt :: Bool -> Bool -> (Plc,ASP_ID) -> SA.Server_Options
gen_asp_server_opt simB debugB (plc,id) =
  gen_server_opt simB debugB (ASP_SERV id) plc

gen_asp_server_opts :: Bool -> Bool -> [(Plc,ASP_ID)] -> [SA.Server_Options]
gen_asp_server_opts simB debugB ps =
  map (gen_asp_server_opt simB debugB) ps

gen_cvm_server_opt :: Bool -> Bool -> Bool -> Plc -> SA.Server_Options
gen_cvm_server_opt simB debugB aspSpawnB p =
  let sig_server_addr = get_server_addr_gen SIGN p
      st = CVM_SERV (CVM_SERV_Params p aspSpawnB (Sign_Server_Addr sig_server_addr)) in
    gen_server_opt simB debugB st p

gen_cvm_server_opts :: Bool -> Bool -> Bool -> [Plc] -> [SA.Server_Options]
gen_cvm_server_opts simB debugB aspSpawnB ps =
  map (gen_cvm_server_opt simB debugB aspSpawnB) ps

gen_par_server_opt :: Bool -> Bool -> Bool -> Plc -> SA.Server_Options
gen_par_server_opt simB debugB aspSpawnB p =
  let sig_server_addr = get_server_addr_gen SIGN p
      st = PAR_SERV (CVM_SERV_Params p aspSpawnB (Sign_Server_Addr sig_server_addr)) in
    gen_server_opt simB debugB st p

gen_par_server_opts :: Bool -> Bool -> Bool -> [Plc] -> [SA.Server_Options]
gen_par_server_opts simB debugB aspSpawnB ps =
  map (gen_par_server_opt simB debugB aspSpawnB) ps

{-
lookupPath :: ServerType -> IO FilePath
lookupPath v = do
  let tag =
        case v of
        {-COMM -> "COMM"
        PAR -> "PAR" -}
        SIGN -> "SIG"
        STORE -> "STORE"
        ASP_SERV i -> "ASP_" ++ (show i)
        CVM_SERV params -> "CVM_" -- ++ (cvm_params_port params)
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
-}










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
