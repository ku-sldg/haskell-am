{-# LANGUAGE ScopedTypeVariables #-}

module IO_Stubs where

import Copland
import BS (BS)
import Term_Defs(ASP_PARAMS(..), RawEv, Plc, Term, Loc, get_bits, EvC(..))
import CryptoImpl (doHash, doSign)
import StVM (CVM)
import CommTypes
import CommUtil
--import Comm


import qualified Data.ByteString as B (readFile, concat)
import Control.Monad.IO.Class (liftIO)
import MonadCop(me, sig_mechanism, nameServer, getTheirSock, getAspSock, simulation)
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (lift)

encodeEvRaw :: RawEv -> BS
encodeEvRaw e = B.concat e

do_asp' :: ASP_PARAMS -> RawEv -> CVM BS
do_asp' params@(Coq_asp_paramsC id _ _ _) e = do --return empty_bs
  let reqm = (AspRequestMessage params e)
  addr <- lift $ getAspSock id
  (rm :: AspResponseMessage) <- liftIO $ gen_run_client addr reqm
  return (aspBits rm)

do_sig' :: BS -> CVM BS
do_sig' bs = do
  simb <- asks simulation
  case simb of
    True -> return bs
    False -> do
      sm <- asks sig_mechanism
      case sm of
        Sign_Server_Addr addr -> do 
          let reqm = (SigRequestMessage bs)
          (rm :: SigResponseMessage) <- liftIO $ gen_run_client addr reqm
          return (sigBits rm)
        Sign_Keypath fp ->
          liftIO $ do
          key_bits <- B.readFile fp
          doSign key_bits bs
  
do_hash' :: BS -> CVM BS
do_hash' bs = return (doHash bs)

do_start_par_thread :: Loc -> Term -> RawEv -> CVM ()
do_start_par_thread loc t e = undefined

doRemote_session' :: Term -> Plc -> EvC -> CVM EvC
doRemote_session' t pTo e = do
  addr <- lift $ getTheirSock pTo
  myPl <- asks me
  ns <- asks nameServer
  let reqm = (RequestMessage pTo myPl ns t (get_bits e))
  (rm :: ResponseMessage) <- liftIO $ gen_run_client addr reqm 
  let res = (Coq_evc (respEv rm) Coq_mt)
  return res

do_wait_par_thread :: Loc -> CVM EvC
do_wait_par_thread l = undefined






















--import Copland
--import CoplandLang_Extracted(ASP_PARAMS(..), RawEv, BS, Plc, EvC(..), Term, Loc)

{-
parallel_vm_thread :: Loc -> EvC
parallel_vm_thread l = undefined
-}

{-
Definition do_asp (params :ASP_PARAMS) (mpl:Plc) (x:Event_ID) : BS.
Admitted.

Definition encodeEvRaw(e:RawEv): BS.
Admitted.

Definition do_sig (bs:BS) (p:Plc) (sigTag:Event_ID) : BS.
Admitted.

Definition do_hash (bs:BS) (p:Plc) : BS.
Admitted.

Definition do_start_par_threadIO (loc:Loc) (t:Term) (e:RawEv) : unit.
Admitted.


Definition doRemote_session (t:Term) (pTo:Plc) (e:EvC) : EvC.
Admitted.

Definition parallel_vm_thread (l:Loc) (t:Term) (p:Plc) (e:EvC) : EvC.
Admitted.
-}


