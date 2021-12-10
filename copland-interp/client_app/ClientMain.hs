{-  Executable that acts as the top-level client, performing the first request(s) of an attestation protocol execution.  Sequences execution of Copland phrases, collects results, and optionally performs appraisal.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland
import MonadCop (runCOP, Cop_Env(..))
--import MonadAM
--import ExecCopland
--import MonadVM_Old
--import Interp (getNameMap,interp)
import Comm (genNameServer)
import ClientProgArgs (getClientOptions, Client_Options(..))
--import qualified Appraise as APP (appraiseUsm)
import qualified CryptoImpl as CI (doHashFile, lookupSecretKeyBytesIO, lookupSecretKeyPath, doNonce)
--import ServerAppUtil(spawn_the_servers)
--import qualified DemoStates as DS (am_env_init, vm_state_init)
import UDcore
import CommUtil
import MonadAM

import Impl_VM_Extracted (run_cvm')
import Term_Defs_Deriving
import StVM_Deriving
import StVM (Coq_cvm_st(..))
--import Term_Defs (AnnoTermPar(..), AnnoTermPar(..),ASP(..),ASP_PARAMS(..),EvC(..), Evidence(..))
import qualified DemoStates as DS
import ServerAppUtil(spawn_servers_term)
import GenServerOpts(get_sample_aspmap, gen_name_map_term)

import Control.Monad.Trans(liftIO)
import Data.List(union)
import Text.Read(readMaybe)
import Crypto.Sign.Ed25519 (SecretKey(..), verify, toPublicKey)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (threadDelay)
import qualified Data.ByteString as B (empty, writeFile)


import Control.Concurrent.STM
--import Numeric.Natural


{-
clientMain :: IO ()
clientMain = do
  opts <- liftIO $ getClientOptions
  let provBool = optProv opts
  case provBool of
   True -> provision
   False -> am_main am_proto_1 DS.am_env_init
-}

main :: IO ()
main = do
  {-
  let t'_par = Coq_aasp_par (ASPC (Coq_asp_paramsC 1 [] 1 1)) --(Coq_aasp_par (CPY))
  let tsig_par = (Coq_aasp_par SIG)
  let t_par = Coq_alseq_par t'_par tsig_par
  --let at_par_term = Coq_aatt_par DS.one_plc t
-}
  
  
  let t' = Coq_asp (ASPC (Coq_asp_paramsC 42 [] 1 1))
  let tsig = Coq_asp SIG 
  let t = Coq_lseq t' tsig
  let at_term = Coq_att 555 {-DS.two_plc-} t

  let at_term' = Coq_att DS.one_plc at_term

  let at_term'' = Coq_lseq at_term' at_term

  let my_term = at_term''
      mypl = DS.zero_plc

  print "BEFORE spawn_servers_term in ClientMain"

  spawn_servers_term False True my_term mypl

  CC.threadDelay 10000
  

  let am_comp = am_run_cvm my_term
  res <- runAM am_comp empty_AM_env empty_AM_state
  
  putStrLn $ "\n" ++ "Term executed: \n" ++ (show my_term) ++ "\n"
  putStrLn $ "Result: \n" ++ (show res) ++ "\n"

{-
  nval <- CI.doNonce
  let st = (Coq_mk_st (Coq_evc [nval] (Coq_nn 1)) [] 0 0)
      --sa = DS.sample_client_args
      --nm = DS.sample_client_name_map
      --nm = gen_name_map_term my_term
      --mypl = DS.zero_plc
      --store = undefined
      --myAsps = get_sample_aspmap my_term mypl --DS.sample_aspmap
      --mySigSock = DS.sig_zero_addr
      --sm = Sign_Server_Addr mySigSock
  let env = DS.sample_cop_env False True my_term mypl
  --Cop_Env False True nm sm mypl store myAsps
    --build_Cop_Env_AM sa nm mypl store myAsps
  putStrLn $ "\n" ++ "Term executed: \n" ++ (show my_term) ++ "\n"
  putStrLn $ "Starting CVM state: \n" ++ (show st) ++ "\n"
  --putStrLn $ "Starting CVM env: \n" ++ (show env) ++ "\n"
  res <- run_cvm' my_term st env -- TODO: get real Cop_Env
  putStrLn $ "Result CVM state: \n" ++ (show res) ++ "\n"
-}




  

{-
{- This is a hard-coded initial state for demo/testing purposes.
   TODO:  Need to make this user-facing, provide an external registration process for ASP components.  -}
am_state_init :: AM_St
am_state_init = 
  let -- Maps place to the asp_id that checks signatures from that place.
      app_sig_map = M.fromList [(0,41)]
      -- Similar map for hash appraisals,
      app_hsh_map = M.empty
      -- Maps a place/asp_id pair to the asp_id that can appraise it.
      app_asp_map = M.fromList [((0, 1),42)]
      -- asp_id for (appraisal) asp that checks nonces
      app_nonceCheckAsp = 0 in
        initial_AM_state app_sig_map app_hsh_map app_asp_map app_nonceCheckAsp

{- This is a hard-coded initial state for demo/testing purposes.
   TODO:  Need to make this user-facing, provide an external registration process for ASP, SIG, and Comm Server components.  -} 
vm_state_init :: Ev -> IO VM_St
vm_state_init e = do
  -- Register Comm Server
  commSocketPath <- lookupPath COMM
  -- Register my (place 0's) Signature server
  sigSocketPath <- lookupPath SIGN
  -- Register ASP 1
  asp1SocketPath <- lookupPath (ASP_SERV 1)
  -- Register appraiser of ASP 1
  app1SocketPath <- lookupPath (ASP_SERV 42)
  -- Register appraiser of 0's signature
  appSig0SocketPath <- lookupPath (ASP_SERV 41) 
  let aspMap =
        M.fromList
        [(1,asp1SocketPath),(42,app1SocketPath),(41,appSig0SocketPath)]
  return $ initial_VM_state e commSocketPath sigSocketPath aspMap
-}

{-        
am_main :: AM EvidenceC  -> AM_Env -> IO ()
am_main proto init = do
  (resEv, resState) <- runAM_with_env proto init
  return ()

{-
runAM_with_st :: AM Ev -> AM_St -> IO (Ev, AM_St)
runAM_with_st am_computation am_st = do
  let fresh_AM_Env = empty_AM_env
      fresh_AM_St = am_st
  runAM am_computation fresh_AM_Env fresh_AM_St
-}
runAM_with_env :: AM EvidenceC  -> AM_Env -> IO (EvidenceC, AM_St)
runAM_with_env am_computation am_env = do
  let da_AM_Env = am_env
      da_AM_St = empty_AM_state
  runAM am_computation da_AM_Env da_AM_St
-}

{-
runAM_fresh :: AM Ev -> IO (Ev, AM_St)
runAM_fresh am_computation = do
  let fresh_AM_Env = empty_AM_env
      fresh_AM_St = empty_AM_state
  runAM am_computation fresh_AM_Env fresh_AM_St
-}

nameMap_from_term :: Term -> IO (M.Map Plc Address)
nameMap_from_term t = do
  let places = getPlaces t
  res <- genNameServer places
  return res

{-
run_vm_t ::  Term -> VM_St -> Cop_Env  -> IO (EvidenceC)
run_vm_t t vm_st cop_env = do
  opts <- getClientOptions
  {-let instrs = (instr_compiler t) -}
  --vm_st <- DS.vm_state_init e
  putStrLn "INVOKING run_vm"
  res <- undefined --run_vm (annotated t) vm_st cop_env
  putStrLn "RAN run_vm"
  return $ st_ev res


get_term_ev :: FilePath -> FilePath -> IO (Term, EvidenceC)
get_term_ev inp einp = do
  t <-
    case inp of
     "" -> return proto1
     _ -> getTerm inp
  ev <-
    case einp of
     "" -> return Mt
     _ -> getEv einp

  before_output t ev
  return (t,ev)

  where
    tm_1 = (ASPT (ASPC (ASP_PARAMSC 1 ["target.txt"] 1 1)))
    ln_1 = (LN tm_1 tm_1)
    brs_1 = (BRS (ALL,NONE) tm_1 tm_1)
    brp_1 = (BRP (ALL,NONE) tm_1 tm_1)
    proto2 = AT 1 (AT 2 ln_1)
    proto3 = (BRP (ALL,NONE) brp_1 brp_1) --AT 1 (AT 2 brp_1)
    proto4 = (BRP (ALL,NONE) proto3 proto3)
    proto1 = AT 1 proto4 --AT 1 proto4
      {-AT 1 (AT 2 ( LN (ASPT (ASPC 1 ["target.txt"]))
                             (ASPT (ASPC 1 ["target.txt"])))) -}
             {-AT 1
             (LN
              (BRP (ALL,NONE) CPY (ASP 1 ["target.txt"]))
              SIG) -}
    before_output :: Term -> EvidenceC -> IO ()
    before_output t ev = do
      putStrLn $ "\n" ++ "Protocol Executed(Also in demoOutput/protoIn.hs): \n" ++ (prettyT t)
      putStrLn $ "\n" ++ "Initial Evidence: \n" ++ (prettyEv ev) ++ "\n"
-}

{-
am_proto_1 :: AM EvidenceC
am_proto_1 = do
  opts <- liftIO $ getClientOptions
  let termFile = optTermIn opts
      evFile = optEvIn opts
      namesFile = optNames opts
      spawnServers = optSpawn opts
      spawnSimBool = optSpawnSim opts
      spawnDebugBool = optSpawnDebug opts
      appraiseBool = optApp opts
      compileBool = optCompile opts

  (t,ev) <- liftIO $ get_term_ev termFile evFile

  {- Uncomment to use generated nonce as initial evidence -}
  n1 <- am_genNonce Mt
  n <- am_genNonce n1
  let ev = Mt --n

  let places = getPlaces t

  nm <- undefined --[] -- <- liftIO $ getNameMap namesFile places

  case spawnServers of
   True -> do
     liftIO $ spawn_the_servers nm spawnSimBool spawnDebugBool compileBool
     liftIO $ CC.threadDelay 10000
   False -> return ()

  let annoT = undefined --annotated t
  liftIO $ putStrLn $ "Annotated term executed: " ++ undefined --(show annoT)

  (reqs,store) <- liftIO $ undefined --derive_comm_reqs annoT nm 0 -- TODO: 0 ok?
  liftIO $ putStrLn $ "Comm reqs/store: " ++ (show (length reqs)) ++ "\n"
  liftIO $ setupComm reqs

  --opts <- getClientOptions
  cop_env <- liftIO $ build_Cop_Env_AM opts nm store
  
  -- if client compiles the received copland term,
  -- and executes the generated sequence of copland instructions.
  resEv <- case compileBool of
             True -> liftIO $ do
               --error "HI"
               vm_st <- DS.vm_state_init ev
               run_vm_t t vm_st cop_env
             -- or simply interprets the received copland term
             False -> do
               --error "HEY"
               undefined --liftIO $ runCOP (interp t ev) cop_env

  liftIO $ putStrLn $ "\n\nEVIDENCE: " ++ (show resEv) ++ "\n\n"

  case appraiseBool of
   True -> do
     let init_ev_type = Nt 1 (Nt 0 Mtt) -- TODO: generalize this
     app_term <- gen_appraisal_term t {-ev-} resEv init_ev_type
     --gen_appraisal_term t 0 ev resEv -- TODO: 0 place ok?

     nonceB <- am_get_app_nonce_bool --check_nonces_ev resEv
     liftIO $ putStrLn $ "app_term: " ++ (show app_term)
     app_ev <- liftIO $ do
       vm_st <- DS.vm_state_init Mt -- TODO: Mt evidence ok?
       run_vm_t app_term vm_st cop_env
       --b <- appraise_proto_1 resEv
     liftIO $ putStrLn $ "appraisal result: " ++ (show app_ev)
     let boolList = appraise_ev app_ev
     let appBoolResult = and (boolList ++ [nonceB])
     liftIO $ putStrLn $ "bool list: " ++ (show boolList)
     liftIO $ putStrLn $ "nonce check: " ++ (show nonceB)
     liftIO $ putStrLn $ "Appraisal result: " ++ (show appBoolResult)
   False -> return ()

  liftIO $ after_output t ev resEv
  return resEv
-}


{-
after_output :: Term -> EvidenceC -> EvidenceC -> IO ()
after_output t ev resEv = do
  opts <- getClientOptions
  let jsonFlag = optJson opts
      outp = optOut opts
      protoInFile = "../demoOutput/protoIn.hs"
      evOutFile = "../demoOutput/protoOut.hs"
      pString = (prettyT t) ++ "\n\n" ++ (prettyEv ev)

  writeFile protoInFile pString
  case jsonFlag of
   True -> jsonOut t ev resEv
   False -> return ()

  case outp of
   "" -> do
     print_write_ev_result resEv "../demoOutput/protoOut.hs"
   fp -> do
     print_write_ev_result resEv fp

   where
     print_write_ev_result :: EvidenceC  -> FilePath -> IO ()
     print_write_ev_result resEv fp = do
       putStrLn $ "\n" ++ "Evidence Result: " ++ "\n" ++ (prettyEv resEv) ++ "\n"
       writeFile fp (prettyEv resEv)

-}

-- Attempt to read a value from the FIRST LINE of a file.
-- Output a type(and filepath)-specific error message upon failure.
readFileGen :: Read a => FilePath -> String -> IO a
readFileGen fp typeString = do
      s <- readFile fp
      let ss = lines s
      let maybeT = readMaybe (head ss)
      case maybeT of
       Just t -> return t
       _ -> error $ "Failed to parse " ++ typeString ++ " from file: " ++ fp
       
getTerm :: FilePath -> IO Term
getTerm fp = readFileGen fp "T"

{-
getEv :: FilePath -> IO EvidenceC 
getEv fp = readFileGen fp "EvidenceC"
-}

{- Hard-coded provisioning.
   TODO: Make this user-facing.
         Probably calls for its own provisioning executable + CLI.  -}
provision :: IO ()
provision = do
  bits <- CI.doHashFile "../target.txt"
  B.writeFile "goldenInputBits.txt" bits

  {-bits <- CI.doHashFile "../kimTarget.txt"
  B.writeFile "goldenKimBits.txt" bits-}

getPlaces :: Term -> [Plc]
getPlaces t = getPlaces' t []

getPlaces' :: Term -> [Plc] -> [Plc]
getPlaces' t pls =
  case t of
   Coq_att p t' -> union [p] (getPlaces' t' pls)
   Coq_lseq t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bseq _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bpar _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   _ -> pls



{-
appraise_proto_1 :: Ev -> AM Bool
appraise_proto_1 e = do
  let (G sigVal e'@(PP n@(N 0 nonceVal (Mt))
           (U 1 args {-["target.txt"]-} hashVal (Mt)))
       ) = e

  kp <- liftIO $ lookupSecretKeyPath
  priKeyBits <- liftIO $ lookupSecretKeyBytesIO kp

  let evBits = encodeEv e'
  let priKey = SecretKey priKeyBits
      pubKey = toPublicKey priKey  --TODO: generalize public key management
      sigResult = verify pubKey sigVal

  liftIO $ putStrLn $ "Sig Check: " ++ (show sigResult)
  (usmCheck,goldenHash) <- liftIO $ APP.appraiseUsm 1 1 args hashVal

  liftIO $ putStrLn $ "USM Check: " ++ (show usmCheck)
  nonceCheck <- am_checkNonce n

  liftIO $ putStrLn $ "Nonce Check: " ++ (show nonceCheck)
  return (sigResult && usmCheck && nonceCheck)
-}
