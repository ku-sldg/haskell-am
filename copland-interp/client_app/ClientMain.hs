{-  Executable that acts as the top-level client, performing the first request(s) of an attestation protocol execution.  Sequences execution of Copland phrases, collects results, and optionally performs appraisal.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland
import MonadCop (runCOP, Cop_Env(..))
--import Comm (genNameServer)
import ClientProgArgs (getClientOptions, Client_Options(..))
import qualified CryptoImpl as CI (doHashFile, lookupSecretKeyBytesIO, lookupSecretKeyPath, doNonce)
import UDcore
import CommUtil
import MonadAM
import MonadAM_Types (empty_AM_env, empty_AM_state, runAM)
import Impl_appraisal_alt(build_app_comp_evC)
import Impl_VM_Extracted (run_cvm')
import Term_Defs_Deriving
import StVM_Deriving
import StVM (Coq_cvm_st(..), st_ev)
import qualified DemoStates as DS
import ServerAppUtil(spawn_servers_terms)
import GenServerOpts(get_sample_aspmap, gen_name_map_term)
import qualified Example_Phrases as EP

import Control.Monad.Trans(liftIO)
import Data.List(union)
import Text.Read(readMaybe)
import Crypto.Sign.Ed25519 (SecretKey(..), verify, toPublicKey)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (threadDelay, forkIO)
import qualified Data.ByteString as B (empty, writeFile)
import Control.Concurrent.STM


main :: IO ()
main = do
  opts <- liftIO $ getClientOptions
  let provBool = optProv opts
  case provBool of
   True -> provision
   False -> clientMain opts

local_term :: Term
local_term = EP.cert_style --EP.cert_style_simple_sig --EP.cert_cache_p0 --EP.cert_cache_p0 --EP.layered_bg_weak --EP.test_par_nested --EP.layered_bg_strong --EP.test_par_nested --EP.layered_bg_strong
  --EP.cert_style
  --EP.layered_bg_strong
  --EP.layered_bg_weak
  --EP.par_mut_p1
  --EP.par_mut_p0
  --EP.bg_check
  --EP.cert_cache_p0
--EP.cert_cache_p1
--EP.cert_style_simple_sig
--EP.cert_style

local_ev :: RawEv
local_ev = []

do_when :: Bool -> IO () -> IO ()
do_when b c =
  if b then c
    else return ()

clientMain :: Client_Options -> IO ()
clientMain (Client_Options
             termFile
             evFile
             outFile
             sim_b
             prov_b
             json_b
             debug_b
             spawnCVM_b
             spawnASP_b
             spawnDebug_b
             namesFile
             appraise_b) = do
      
  (my_term, my_ev) <- get_term_ev termFile evFile
  let  mypl = DS.zero_plc -- TODO: ok to hardcode?
       multi_termB = False

       my_terms =
         case multi_termB of
           True -> [my_term, EP.cert_cache_p1]
           False -> [my_term]
       my_places =
         case multi_termB of
           True -> [mypl, DS.one_plc]
           False -> [mypl]

  do_when (spawnCVM_b || spawnASP_b) $ do
    print "BEFORE spawn_servers_term in ClientMain"
    spawn_servers_terms sim_b debug_b spawnCVM_b spawnASP_b my_terms  my_places
    CC.threadDelay 10000


  

  let am_comp =
        am_run_cvm_nonce_init
          sim_b
          debug_b
          spawnCVM_b
          spawnASP_b
          mypl
          my_term

      am_comp_1 =
        am_run_cvm_nonce_init
          sim_b
          debug_b
          spawnCVM_b
          spawnASP_b
          DS.one_plc
          EP.cert_cache_p1

  do_when (multi_termB) $ do
    CC.forkIO $ do
      runAM am_comp_1 empty_AM_env empty_AM_state
      return ()
    return ()
  
          
 -- res@(cvmst_res, amst_res) <- runAM am_comp empty_AM_env empty_AM_state
  res@((nonce_id, rawev_res), amst_res) <- runAM am_comp empty_AM_env empty_AM_state

  putStrLn $ "\n" ++ "Term executed: \n" ++ (show my_term) ++ "\n"
  putStrLn $ "Result: \n" ++ (show res) ++ "\n"

  do_when (appraise_b) $ do
    let --cvmst_res = fst res
        --rawev_res = get_bits (st_ev cvmst_res)
        et_app = eval my_term mypl (Coq_nn nonce_id)
    putStrLn $ show et_app
    let appraise_comp =
          do
            liftIO $ putStrLn $ "RAWEV LEN: " ++ (show (length rawev_res))
            liftIO $ putStrLn $ "et_size: " ++ (show (et_size et_app))
            v <- build_app_comp_evC et_app rawev_res
            return v
    --let new_app_st = snd res
    app_res <- runAM appraise_comp empty_AM_env amst_res

    putStrLn $ "Appraise Result: \n" ++ (show app_res) ++ "\n"

    let app_res_bool = certWalk_EvidenceC (fst app_res)
    putStrLn $ "Walked EvidenceC Appraise Bool Result: \n" ++ (show app_res_bool)


get_term_ev :: FilePath -> FilePath -> IO (Term, RawEv)
get_term_ev inp einp = do
  t <-
    case inp of
     "" -> return local_term
     _ -> getTerm inp
  ev <-
    case einp of
     "" -> return local_ev
     _ -> getEv einp
     
  return (t,ev)


-- Attempt to read a value from the FIRST LINE of a file.
-- Output a type(and filepath)-specific error message upon failure.
readFileGen :: Read a => FilePath -> String -> IO a
readFileGen fp typeString = do
      s <- readFile fp
      let ss = lines s
      let maybeT = readMaybe (head ss)
      case maybeT of
       Just t -> return t
       _ -> error $
         "Failed to parse value of type '" ++ typeString ++
         "' from file: " ++ fp
       
getTerm :: FilePath -> IO Term
getTerm fp = readFileGen fp "Term_Defs.Term"

getEv :: FilePath -> IO RawEv
getEv fp = readFileGen fp "Term_Defs.RawEv"



{- Hard-coded provisioning.
   TODO: Make this user-facing.
         Probably calls for its own provisioning executable + CLI.  -}
provision :: IO ()
provision = do
  bits <- CI.doHashFile "../target.txt"
  B.writeFile "goldenInputBits.txt" bits



{-
  let t' = Coq_asp (ASPC (Coq_asp_paramsC 42 [] 1 1))
      tsig = Coq_asp SIG 
      t = Coq_lseq t' tsig
      at_term = Coq_att 555 {-DS.two_plc-} t

      at_term' = Coq_att DS.one_plc at_term

      at_term'' = Coq_lseq at_term' at_term in
    at_term''
-}
      {-
  let t'_par = Coq_aasp_par (ASPC (Coq_asp_paramsC 1 [] 1 1)) --(Coq_aasp_par (CPY))
  let tsig_par = (Coq_aasp_par SIG)
  let t_par = Coq_alseq_par t'_par tsig_par
  --let at_par_term = Coq_aatt_par DS.one_plc t
-}
