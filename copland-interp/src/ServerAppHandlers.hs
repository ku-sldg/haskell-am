module ServerAppHandlers where

import Copland
import BS (empty_bs)
import StVM
import StVM_Deriving
import MonadCop (Cop_Env(..))
import CommTypes
--import DemoStates (sample_aspmap)
import Impl_VM_Extracted (run_cvm_loc)
import CryptoImpl(doSignD, get_key_simpl)
import GenServerOpts (get_sample_aspmap, par_server_addr)
import qualified ServerProgArgs as SA

import qualified Data.Map as M (empty, insert, lookup, Map)
import Control.Concurrent.STM


build_cvm_config :: CVM_SERV_Params -> SA.Server_Options -> Term -> M.Map Plc Address -> RawEv ->
                    (Cop_Env, Coq_cvm_st)
build_cvm_config params opts t nm e =
  let store = M.empty
      me = cvm_params_plc params
      sm = cvm_params_sig_mech params
      aspmap = get_sample_aspmap t me
      simb = SA.server_optSim opts
      debugb = SA.server_optDebug opts
      par_addr = par_server_addr me

      env = Cop_Env simb debugb nm sm me store aspmap par_addr
      st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0) in
            -- TODO: remove verification params from cvm state?
    (env,st)

  
mod_rawev_map :: TVar (M.Map Loc RawEv) -> Loc -> RawEv -> STM ()
mod_rawev_map mv loc e = modifyTVar mv (M.insert loc e)
  
handle_par_req :: CVM_SERV_Params -> SA.Server_Options -> TVar (M.Map Loc RawEv) ->
                  StartMessagePar -> IO ()
handle_par_req params opts store_var msg@(StartMessagePar loc nm t e) = do
  
  let (env,st) = build_cvm_config params opts t nm e

  putStrLn $ "init state PAR with LOC " ++ (show loc) ++ ": " ++ (show st)
  --print st 
  res_rawev <- {-run_cvm_rawev-}run_cvm_loc t st env

  atomically $ mod_rawev_map store_var loc res_rawev


hpw' :: TVar (M.Map Loc RawEv) -> Loc -> STM RawEv
hpw' store_var loc = do
  m <- readTVar store_var
  let maybe_ev = M.lookup loc m
  case maybe_ev of
    Just e -> return e -- TODO: necessary to delete loc entry here?
    Nothing -> retry

handle_par_wait :: TVar (M.Map Loc RawEv) -> WaitMessagePar -> IO ResponseMessagePar
handle_par_wait store_var msg@(WaitMessagePar loc) = do
  putStrLn $ "HANDLING PAR WAIT for Loc: " ++ (show loc)
  res_ev <- atomically $ hpw' store_var loc
  return (ResponseMessagePar res_ev)



handle_par_init :: TVar Loc -> InitMessagePar -> IO AckInitMessagePar
handle_par_init loc_var msg@(InitMessagePar tSize) = do

  putStrLn "in handle_par_init"
  new_v <- atomically $ do
    v <- readTVar loc_var
    --let v' = (v + tSize)
    writeTVar loc_var (v + tSize)
    return v

  return $ AckInitMessagePar new_v
  



handle_asp :: SA.Server_Options -> AspRequestMessage -> IO AspResponseMessage
handle_asp opts msg@(AspRequestMessage _ _) = do
  let sBits = empty_bs
  return (AspResponseMessage sBits)


handle_remote :: CVM_SERV_Params -> SA.Server_Options -> {-Bool -> Bool -> -}
                 RequestMessage -> IO ResponseMessage
handle_remote params opts rreq@(RequestMessage pTo pFrom nm t e) = do

  print "received RequestMessage: "
  print rreq

  let (env,st) = build_cvm_config params opts t nm e

  print "init state: "
  print st 
  res_rawev <- {-run_cvm_rawev-} run_cvm_loc t st env

  let rm = (ResponseMessage pFrom pTo res_rawev)
  return rm


handle_sig :: SA.Server_Options -> SigRequestMessage -> IO SigResponseMessage
handle_sig opts msg@(SigRequestMessage eBits) = do
  kb <- get_key_simpl "../keys/key0.txt" -- TODO no hardcode
  sBits <- doSignD kb eBits
  return (SigResponseMessage sBits)
