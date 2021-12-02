{-# LANGUAGE ScopedTypeVariables #-}

module IO_Stubs where

import Copland
import BS (BS, empty_bs)
import Term_Defs(ASP_PARAMS(..), RawEv, Plc, EvC(..), Term, Loc, get_bits, Evidence(..), EvC(..))
import CryptoImpl (doHash, doSign)
import System.IO.Unsafe (unsafePerformIO)
import StVM (CVM)
import CommTypes
import CommImpl
import UDcore


import qualified Data.ByteString as B (ByteString, readFile, concat)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import MonadCop(lookupSecretKeyBytes, myKeyPath, me, sig_socket, nameServer, getTheirSock, getAspSock)
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (lift)
import qualified Network.Socket as NS
import qualified Data.Map as M(Map)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.Aeson as DA (encode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL (toStrict)

encodeEvRaw :: RawEv -> BS
encodeEvRaw e = B.concat e

do_asp' :: ASP_PARAMS -> RawEv -> CVM BS
do_asp' params@(Coq_asp_paramsC id _ _ _) e = do   --return empty_bs
  let reqm = (AspRequestMessage params e)
  addr <- lift $ getAspSock id
  (rm :: AspResponseMessage) <- liftIO $ gen_run_client addr reqm
  return (aspBits rm)

{-
lookupSecretKeyBytesIO :: FilePath -> IO BS
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs
-}

{-
get_key_simpl :: IO BS
get_key_simpl = do
  --kp <- lookupSecretKeyPath
  let kp = "./key0.txt" in
    lookupSecretKeyBytesIO kp
-}


{-
  addr <- lift $ getTheirSock pTo
  myPl <- asks me
  ns <- asks nameServer
  let reqm = (RequestMessage pTo myPl ns t (get_bits e))
  (rm :: ResponseMessage) <- liftIO $ runUnixDomainClient addr (sendRec' reqm)
  let res = (Coq_evc (respEv rm) Coq_mt)
  return res
-}

do_sig' :: BS -> CVM BS
do_sig' bs = do --liftIO $ do
  addr <- asks sig_socket
  let reqm = (SigRequestMessage bs)
  (rm :: SigResponseMessage) <- liftIO $ gen_run_client addr reqm --runUnixDomainClient addr (sendRec' reqm)
  --let res = (sigBits rm)
  return (sigBits rm)

  {-
  fp <- asks myKeyPath
  key_bits <- liftIO $ B.readFile fp --lookupSecretKeyBytes --get_key_simpl
  liftIO $ doSign key_bits bs
-}


{-
do_sig' :: BS -> CVM BS
do_sig' bs = do --liftIO $ do
  fp <- asks myKeyPath
  key_bits <- liftIO $ B.readFile fp --lookupSecretKeyBytes --get_key_simpl
  liftIO $ doSign key_bits bs
-}


{-
do_sig :: BS -> BS
do_sig bs = unsafePerformIO (do_sig_IO bs)
-}
  
do_hash' :: BS -> CVM BS
do_hash' bs = return (doHash bs)

do_start_par_thread :: Loc -> Term -> RawEv -> CVM ()
do_start_par_thread loc t e = undefined


gen_client_session :: BS -> NS.Socket -> IO BS
gen_client_session msg conn = do
  NBS.sendAll conn msg
  NBS.recv conn 2048

{-
sendRec' :: Plc -> Plc -> M.Map Plc Address ->
           Term -> RawEv -> BS
sendRec' pTo pFrom namesFrom t e =
  let rm = (RequestMessage pTo pFrom namesFrom t e)
      messageBits = DA.encode rm in
    BL.toStrict messageBits
-}


--(ToJSON a,FromJSON a,Read a,Show a{-,Arbitrary a-}) =>

sendRec' :: (DA.ToJSON a,DA.FromJSON a,DA.ToJSON b,DA.FromJSON b) =>
            a -> NS.Socket -> IO b
sendRec' rm conn = do
  --let msg = sendRec' pTo pFrom namesFrom t e
  let messageBits = DA.encode rm
      msg = BL.toStrict messageBits
  msg' <- gen_client_session msg conn
  decodeGen msg'
  --res <- decodeGen msg' --(res :: ResponseMessage) <- decodeGen msg'
  --return res


{-
sendRec :: RequestMessage -> NS.Socket -> IO ResponseMessage
sendRec rm conn = do
  --let msg = sendRec' pTo pFrom namesFrom t e
  let messageBits = DA.encode rm
      msg = BL.toStrict messageBits
  msg' <- gen_client_session msg conn
  res <- decodeGen msg' --(res :: ResponseMessage) <- decodeGen msg'
  return res
-}

{-
sendRec :: Plc -> Plc -> M.Map Plc Address ->
           Term -> RawEv -> NS.Socket -> IO ResponseMessage
sendRec pTo pFrom namesFrom t e conn = do
  let msg = sendRec' pTo pFrom namesFrom t e
  msg' <- gen_client_session msg conn
  (res :: ResponseMessage) <- decodeGen msg'
  return res
-}

gen_run_client ::  (DA.ToJSON a,DA.FromJSON a,DA.ToJSON b,DA.FromJSON b) =>
                   Address -> a -> IO b
gen_run_client addr reqm = do
  runUnixDomainClient addr (sendRec' reqm)
  
  

doRemote_session' :: Term -> Plc -> EvC -> CVM EvC
doRemote_session' t pTo e = do
  addr <- lift $ getTheirSock pTo
  myPl <- asks me
  ns <- asks nameServer
  let reqm = (RequestMessage pTo myPl ns t (get_bits e))
  (rm :: ResponseMessage) <- liftIO $ gen_run_client addr reqm  --runUnixDomainClient addr (sendRec' reqm)
  let res = (Coq_evc (respEv rm) Coq_mt)
  return res

  
  --return undefined

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


