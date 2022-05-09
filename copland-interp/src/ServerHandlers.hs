{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

module ServerHandlers where

import Copland
import BS (BS, empty_bs, zero_bs, one_bs, bool_to_bs)
import ConcreteEvidence --(EvidenceC)
import Cvm_St
import StVM_Deriving
import MonadCop (Cop_Env(..))
import CommTypes
--import DemoStates (sample_aspmap)
import Impl_CVM_Wrapper (run_cvm_loc)
import CryptoImpl(doSignD, get_key_simpl)
import ServerOpts (get_sample_aspmap, par_server_addr, sig_server_addr)
import qualified ServerProgArgs as SA
import qualified Example_Phrases_Admits as EPA (store_args, retrieve_args, app_bg_weak_args, app_bg_strong_args)
import qualified DemoStates as DS
import Impl_appraisal(build_app_comp_evC)
import MonadAM_Types (AM_St(..), empty_AM_env, runAM)

import qualified Copland_Concrete as CT
import qualified Example_Phrases_Concrete as EPC

import qualified Data.Map as M (empty, insert, lookup, delete, Map, fromList)
import Control.Concurrent.STM
import qualified Data.Aeson as DA (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as BIN
import Control.Monad.IO.Class (liftIO)


build_cvm_config :: CVM_SERV_Params -> SA.Server_Options -> Term -> {-M.Map Plc Address -> -} RawEv ->
                    (Cop_Env, Coq_cvm_st)
build_cvm_config params@(CVM_SERV_Params me aspSpawn_b _) opts t {-nm-} e =
  let --store = M.empty
      --me = cvm_params_plc params
      {-
      sm = cvm_params_sig_mech params
      aspmap = get_sample_aspmap t me
-}
      --cvmSpawn_b = cvm_asps_simb
      simb = SA.server_optSim opts
      debugb = SA.server_optDebug opts
      --par_addr = par_server_addr me

      cvmSpawn_b = True -- TODO: make this part of CVM_SERV_Params

      --env = Cop_Env simb debugb nm sm me store aspmap par_addr
      env = DS.sample_cop_env simb debugb cvmSpawn_b aspSpawn_b t me
      st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0) in
            -- TODO: remove verification params from cvm state?
    (env,st)

  
mod_rawev_map :: TVar (M.Map Loc RawEv) -> Loc -> RawEv -> STM ()
mod_rawev_map mv loc e = modifyTVar mv (M.insert loc e)

clear_rawev_map_at_loc :: TVar (M.Map Loc RawEv) -> Loc -> STM ()
clear_rawev_map_at_loc mv loc = modifyTVar mv (M.delete loc)
  
handle_par_req :: CVM_SERV_Params -> SA.Server_Options -> TVar (M.Map Loc RawEv) ->
                  StartMessagePar -> IO ()
handle_par_req params opts store_var msg@(StartMessagePar loc _ {-nm-} t e) = do
  
  let (env,st) = build_cvm_config params opts t {-nm-} e

  putStrLn $ "init state PAR with LOC " ++ (show loc) ++ ": " ++ (show st)
  --print st 
  res_rawev <- {-run_cvm_rawev-}run_cvm_loc t st env

  atomically $ mod_rawev_map store_var loc res_rawev


hpw' :: TVar (M.Map Loc RawEv) -> TVar [Loc] -> Loc -> STM RawEv
hpw' store_var locs_var loc = do
  m <- readTVar store_var
  let maybe_ev = M.lookup loc m
  case maybe_ev of
    Just e -> do
      modifyTVar locs_var (enque_loc loc)
       -- TODO: necessary to clear loc entry from store here?
      clear_rawev_map_at_loc store_var loc 
      return e
    Nothing -> retry

handle_par_wait :: TVar (M.Map Loc RawEv) -> TVar [Loc] -> WaitMessagePar -> IO ResponseMessagePar
handle_par_wait store_var locs_var msg@(WaitMessagePar loc) = do
  putStrLn $ "HANDLING PAR WAIT for Loc: " ++ (show loc)
  res_ev <- atomically $ hpw' store_var locs_var loc
  putStrLn $ "RECEIVED PAR WAIT for Loc: " ++ (show loc)
  return (ResponseMessagePar res_ev)

enque_loc :: Loc -> [Loc] -> [Loc]
enque_loc loc ls = ls ++ [loc]

handle_par_init :: TVar [Loc] -> InitMessagePar -> IO AckInitMessagePar
handle_par_init locs_var msg@(InitMessagePar tSize) = do

  putStrLn "in handle_par_init"
  new_locs <- atomically $ do
    ls <- readTVar locs_var
    --let v' = (v + tSize)
    if (length ls < tSize)
      then retry
      else do 
      let res = take tSize ls
          rem = drop tSize ls
      writeTVar locs_var rem
      return res

  putStrLn "leaving handle_par_init"
  return $ AckInitMessagePar new_locs


handle_asp_attest :: AspRequestMessage -> IO AspResponseMessage
handle_asp_attest msg@(AspRequestMessage (Coq_asp_paramsC _ args _ _) rawEv) = do
  let me = 0 -- TODO: no hardcode?
      aspsimb = False
      sigmech = Sign_Server_Addr (sig_server_addr me)
      params = CVM_SERV_Params me aspsimb sigmech
      optSim = False
      optDebug = False
      sport = "" -- TODO: is this ever used?
      sType = CVM_SERV params
      opts = SA.Server_Options optSim optDebug sport sType
      t = Coq_asp SIG --Coq_asp (ASPC (Coq_asp_paramsC 42 [] me 42)) -- TODO: is this dummy term ok?
      initEv = last rawEv
      (env,st) = build_cvm_config params opts t {-nm-} [initEv]

  res_rawev <- {-run_cvm_rawev-} run_cvm_loc t st env

  let att_res = AttestResult t res_rawev
      resp_bs = BL.toStrict $ BIN.encode att_res

  putStrLn $ "AttestResult computed: " ++ (show att_res)


  {-
  let (bs::BS.BS) = head rawEv
      lazy_bs = BL.fromStrict bs
      (t::Term) = BIN.decode lazy_bs
-}
  return $ AspResponseMessage resp_bs --BS.empty_bs

appraise_attest_result :: [BS] -> IO BS
appraise_attest_result rawEv = do
  let (bs::BS.BS)= head rawEv
      lazy_bs = BL.fromStrict bs
      err_str = typed_error_str "AttestResult"
      (r@(AttestResult t res_rawev)::AttestResult) = decodeBin err_str lazy_bs
      nval = last rawEv
  putStrLn $ "Nonce GRABBEDD: " ++ (show nval)
  --putStrLn $ "AttestResult grabbed: " ++ (show r)
  let them = 0 -- TODO: no hardcode?
      init_ev_type = (Coq_nn 0) -- TODO: ok?
      (et_app::Evidence) = eval t them init_ev_type
  --putStrLn $ "HERE" ++ (show et_app)
  putStrLn $ "Evidence Type computed for appraise ASP: " ++ (show et_app)
  let appraise_comp = build_app_comp_evC et_app res_rawev
      amst = AM_St (M.fromList [(0,nval{-BS.empty_bs-})]) 1 -- TODO: need actual nonce value from attester/relying?

  ((app_res, _)::(EvidenceC, AM_St)) <- runAM appraise_comp empty_AM_env amst

  putStrLn $ "Appraisal EvidenceC structure computed: " ++ (show app_res)

  let resp_bs = BL.toStrict $ BIN.encode app_res
  return resp_bs

handle_asp_appraise :: AspRequestMessage -> IO AspResponseMessage
handle_asp_appraise msg@(AspRequestMessage (Coq_asp_paramsC _ args _ _) rawEv) =

    if | ((args == EPA.app_bg_weak_args) || (args == EPA.app_bg_strong_args)) -> do
           let nval = last rawEv
               them = 0
               init_ev_type = (Coq_nn 0)
               t =
                 case (args == EPA.app_bg_weak_args) of
                   True -> CT.toExtractedTerm EPC.layered_bg_weak_prefix
                   False -> CT.toExtractedTerm EPC.layered_bg_strong_prefix
               (et_app :: Evidence) = eval t them init_ev_type
               appraise_comp = build_app_comp_evC et_app rawEv
               amst = AM_St (M.fromList [(0,nval{-BS.empty_bs-})]) 1 -- TODO: need actual nonce value from attester/relying?

           ((app_res, _)::(EvidenceC, AM_St)) <- runAM appraise_comp empty_AM_env amst

           putStrLn $ "Appraisal EvidenceC structure computed for app_bg_weak: " ++ (show app_res)

           let resp_bs = BL.toStrict $ BIN.encode app_res
      
           return $ AspResponseMessage resp_bs
   
       | otherwise -> do
           putStrLn $ "starting OTHER appraisal (not app_bg_weak)"
           resp_bs <- appraise_attest_result rawEv
           putStrLn $ "returning from OTHER appraisal (not app_bg_weak)"
           return $ AspResponseMessage resp_bs

           {-
  
         let (bs::BS.BS)= head rawEv
             lazy_bs = BL.fromStrict bs
             (r@(AttestResult t res_rawev)::AttestResult) = BIN.decode lazy_bs
             nval = last rawEv
         putStrLn $ "Nonce GRABBEDD: " ++ (show nval)
         --putStrLn $ "AttestResult grabbed: " ++ (show r)
         let them = 0 -- TODO: no hardcode?
             init_ev_type = (Coq_nn 0) -- TODO: ok?
             (et_app::Evidence) = eval t them init_ev_type
         --putStrLn $ "HERE" ++ (show et_app)
         putStrLn $ "Evidence Type computed for appraise ASP: " ++ (show et_app)
         let appraise_comp = build_app_comp_evC et_app res_rawev
             amst = AM_St (M.fromList [(0,nval{-BS.empty_bs-})]) 1 -- TODO: need actual nonce value from attester/relying?

         ((app_res, _)::(EvidenceC, AM_St)) <- runAM appraise_comp empty_AM_env amst

         putStrLn $ "Appraisal EvidenceC structure computed: " ++ (show app_res)

         let resp_bs = BL.toStrict $ BIN.encode app_res 
      
         return $ AspResponseMessage resp_bs -}

handle_asp_certify :: AspRequestMessage -> IO AspResponseMessage
handle_asp_certify msg@(AspRequestMessage (Coq_asp_paramsC _ args _ _) rawEv) = do
  let (bs::BS.BS)= head rawEv
      lazy_bs = BL.fromStrict bs
      err_str = typed_error_str "EvidenceC"
      (r::EvidenceC) = decodeBin err_str lazy_bs

  putStrLn $ "Appraisal EvidenceC structure received by Cert ASP: " ++ (show r)
      -- TODO: walk EvidenceC structure for legit certify?
  let b = certWalk_EvidenceC r
  liftIO $ putStrLn $ "Performing Cert ASP, bool result: " ++ show (b)
  let cert_res = BS.bool_to_bs b --r --BS.one_bs  --TODO:  hard-coded true("good") value
      -- TODO: sign cert structure?
      resp_bs = cert_res --BL.toStrict $ cert_res --BIN.encode cert_res
      
  return $ AspResponseMessage resp_bs

handle_asp_cache :: TVar (Maybe RawEv) -> AspRequestMessage -> IO AspResponseMessage
handle_asp_cache cache_var msg@(AspRequestMessage (Coq_asp_paramsC _ args _ _) rawEv) =
  if | args == EPA.store_args -> do
         atomically $ writeTVar cache_var (Just rawEv)
         return $ AspResponseMessage BS.empty_bs
     | args == EPA.retrieve_args -> do
         putStrLn "HERE!!"
         atomically $ do
           mv <- readTVar cache_var
           case mv of
             Nothing -> do
               --liftIO $ putStrLn $ "no cache_var val to read"
               --return $ AspResponseMessage BS.empty_bs
               --error "no cache_var val to read"
               retry
             Just v ->
               --error "read cache_var sucessfully"
               return $ AspResponseMessage (BL.toStrict (DA.encode v))
   
     | otherwise -> error "Unrecocnized args for cache ASP!"

handle_asp_default :: ASP_ID -> {-TVar (Maybe RawEv) -> SA.Server_Options ->-} AspRequestMessage -> IO AspResponseMessage
handle_asp_default asp_id {-cache_var-} {-opts-} msg@(AspRequestMessage _ _) = do
  {-
  if | asp_id == EPA.cache_id -> do
         {-
         cache_var <- newTVarIO Nothing
         putStrLn "created new TVar for cache store..."
-}
         handle_asp_cache cache_var msg
     | otherwise -> do
-}
         putStrLn "Running simulated ASP..."
         -- TODO:  faking un-implemented ASPs for now...
         let sBits = empty_bs
         return (AspResponseMessage sBits)


handle_remote :: CVM_SERV_Params -> SA.Server_Options -> {-Bool -> Bool -> -}
                 RequestMessage -> IO ResponseMessage
handle_remote params opts rreq@(RequestMessage pTo pFrom _ {-nm-} t e) = do

  print "received RequestMessage: "
  print rreq

  let (env,st) = build_cvm_config params opts t {-nm-} e

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
