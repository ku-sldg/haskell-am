module ServerAppHandlers where

import Copland
import BS (empty_bs)
import StVM
import StVM_Deriving
import MonadCop (Cop_Env(..))
import CommTypes
--import DemoStates (sample_aspmap)
import Impl_VM_Extracted (run_cvm_rawev)
import CryptoImpl(doSignD, get_key_simpl)
import GenServerOpts (get_sample_aspmap)
import qualified ServerProgArgs as SA

import qualified Data.Map as M (empty, Map)
import Control.Concurrent.STM


handle_par_req :: CVM_SERV_Params -> SA.Server_Options -> TMVar (M.Map Loc RawEv) ->
                  StartMessagePar -> IO ()
handle_par_req params opts store msg@(StartMessagePar loc nm t e) =
  return () -- TODO: fill in real impl here

handle_par_wait :: TMVar (M.Map Loc RawEv) -> WaitMessagePar -> IO ResponseMessagePar
handle_par_wait store msg@(WaitMessagePar loc) =
  return (ResponseMessagePar []) -- TODO: fill in real impl here 


handle_asp :: SA.Server_Options -> AspRequestMessage -> IO AspResponseMessage
handle_asp opts msg@(AspRequestMessage _ _) = do
  let sBits = empty_bs
  return (AspResponseMessage sBits)


handle_remote :: CVM_SERV_Params -> SA.Server_Options -> {-Bool -> Bool -> -}
                 RequestMessage -> IO ResponseMessage
handle_remote params opts rreq@(RequestMessage pTo pFrom names t e) = do

  print "received RequestMessage: "
  print rreq
  
  let store = M.empty
      me = cvm_params_plc params
      sm = cvm_params_sig_mech params
      aspmap = get_sample_aspmap t me
      simb = SA.server_optSim opts
      debugb = SA.server_optDebug opts
  
  let env = Cop_Env simb debugb names sm me store aspmap
  let st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0)
  -- TODO: remove verification params from cvm state?

  print "init state: "
  print st 
  res_rawev <- run_cvm_rawev t st env

  let rm = (ResponseMessage pFrom pTo res_rawev)
  return rm


handle_sig :: SA.Server_Options -> SigRequestMessage -> IO SigResponseMessage
handle_sig opts msg@(SigRequestMessage eBits) = do
  kb <- get_key_simpl "../keys/key0.txt" -- TODO no hardcode
  sBits <- doSignD kb eBits
  return (SigResponseMessage sBits)
