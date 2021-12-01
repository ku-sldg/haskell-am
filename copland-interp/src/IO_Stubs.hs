module IO_Stubs where

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
import MonadCop(lookupSecretKeyBytes, myKeyPath, me, nameServer, getTheirSock)
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (lift)
import qualified Network.Socket as NS
import qualified Data.Map as M(Map)

encodeEvRaw :: RawEv -> BS
encodeEvRaw e = B.concat e

do_asp' :: ASP_PARAMS -> RawEv -> CVM BS
do_asp' params e = return empty_bs


lookupSecretKeyBytesIO :: FilePath -> IO BS
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs

{-
get_key_simpl :: IO BS
get_key_simpl = do
  --kp <- lookupSecretKeyPath
  let kp = "./key0.txt" in
    lookupSecretKeyBytesIO kp
-}

do_sig' :: BS -> CVM BS
do_sig' bs = do --liftIO $ do
  fp <- asks myKeyPath
  key_bits <- liftIO $ B.readFile fp --lookupSecretKeyBytes --get_key_simpl
  liftIO $ doSign key_bits bs
{-
do_sig :: BS -> BS
do_sig bs = unsafePerformIO (do_sig_IO bs)
-}
  
do_hash' :: BS -> CVM BS
do_hash' bs = return (doHash bs)

do_start_par_thread :: Loc -> Term -> RawEv -> CVM ()
do_start_par_thread loc t e = undefined

sendRec :: Plc -> Plc -> M.Map Plc Address ->
           Term -> RawEv -> NS.Socket -> IO ResponseMessage
sendRec pTo pFrom namesFrom t e conn = do
  sendReq pTo pFrom namesFrom t e conn
  receiveResp conn pFrom

doRemote_session' :: Term -> Plc -> EvC -> CVM EvC
doRemote_session' t pTo e = do
  addr <- lift $ getTheirSock pTo
  myPl <- asks me
  ns <- asks nameServer
  rm <- liftIO $ runUnixDomainClient addr (sendRec pTo myPl ns t (get_bits e))
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


