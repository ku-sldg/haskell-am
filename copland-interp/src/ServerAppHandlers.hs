module ServerAppHandlers where

import Copland
import BS (empty_bs)
import StVM
import StVM_Deriving
import MonadCop (build_Cop_Env)
import CommTypes
import DemoStates (sample_aspmap)
import Impl_VM_Extracted (run_cvm_rawev)
import CryptoImpl(doSign, get_key_simpl)

import qualified Data.Map as M (empty)


handle_asp :: AspRequestMessage -> IO AspResponseMessage
handle_asp msg@(AspRequestMessage _ _) = do
  let sBits = empty_bs
  return (AspResponseMessage sBits)



handle_remote :: CVM_SERV_Params -> Bool -> Bool ->
                 RequestMessage -> IO ResponseMessage
handle_remote params b d rreq@(RequestMessage pTo pFrom names t e) = do
  --rreq@(RequestMessage pTo pFrom names t e) <- decodeGen msg

  print "received RequestMessage: "
  print rreq
  let store = M.empty
      me = cvm_params_plc params
      ss = cvm_params_sig_port params
      --me = get_my_pl opts
      --ss = get_my_sig_sock opts
  
  env <- build_Cop_Env b d names me store sample_aspmap ss
  let st = (Coq_mk_st (Coq_evc e (Coq_mt)) [] me 0)

  print "init state: "
  print st 
  res_rawev <- run_cvm_rawev t st env

  let rm = (ResponseMessage pFrom pTo res_rawev)
  return rm


handle_sig :: SigRequestMessage -> IO SigResponseMessage
handle_sig msg@(SigRequestMessage eBits) = do
  --msg <- NBS.recv conn 2048
  --(SigRequestMessage eBits) <- decodeGen msg

  {-
  kp <- lookupSecretKeyPath
  priKeyBits <- lookupSecretKeyBytesIO kp
  let priKey = SecretKey priKeyBits
      sig = dsign priKey eBits
      sigBS = unSignature sig

  
  let sBits = sigBS
  -}
  kb <- get_key_simpl
  sBits <- doSign kb eBits
  return (SigResponseMessage sBits)
