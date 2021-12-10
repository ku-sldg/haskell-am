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

import qualified Data.Map as M (empty)


handle_asp :: AspRequestMessage -> IO AspResponseMessage
handle_asp msg@(AspRequestMessage _ _) = do
  let sBits = empty_bs
  return (AspResponseMessage sBits)


handle_remote :: CVM_SERV_Params -> Bool -> Bool ->
                 RequestMessage -> IO ResponseMessage
handle_remote params b d rreq@(RequestMessage pTo pFrom names t e) = do

  print "received RequestMessage: "
  print rreq
  let store = M.empty
      me = cvm_params_plc params
      sm = cvm_params_sig_mech params
      aspmap = get_sample_aspmap t me
  
  let env = Cop_Env b d names sm me store aspmap
  let st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0)

  print "init state: "
  print st 
  res_rawev <- run_cvm_rawev t st env

  let rm = (ResponseMessage pFrom pTo res_rawev)
  return rm


handle_sig :: SigRequestMessage -> IO SigResponseMessage
handle_sig msg@(SigRequestMessage eBits) = do
  kb <- get_key_simpl "../keys/key0.txt" -- TODO no hardcode
  sBits <- doSignD kb eBits
  return (SigResponseMessage sBits)
