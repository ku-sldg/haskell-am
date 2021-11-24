module IO_Stubs where

--import Copland
--import CoplandLang_Extracted(ASP_PARAMS(..), RawEv, BS, Plc, EvC(..), Term, Loc)
import BS (BS)
import Term_Defs(ASP_PARAMS(..), RawEv, Plc, EvC(..), Term, Loc)
import CryptoImpl (doHash, doSign)
import System.IO.Unsafe (unsafePerformIO)
import StVM (CVM)
import qualified Data.ByteString as B (ByteString, readFile, concat)
import Control.Monad.IO.Class (liftIO)

encodeEvRaw :: RawEv -> BS
encodeEvRaw e = B.concat e

do_asp' :: ASP_PARAMS -> CVM BS
do_asp' params = undefined


lookupSecretKeyBytesIO :: FilePath -> IO BS
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs

get_key_simpl :: IO BS
get_key_simpl = do
  --kp <- lookupSecretKeyPath
  let kp = "./key0.txt" in
    lookupSecretKeyBytesIO kp

do_sig' :: BS -> CVM BS
do_sig' bs = liftIO $ do
  key_bits <- get_key_simpl
  doSign key_bits bs
{-
do_sig :: BS -> BS
do_sig bs = unsafePerformIO (do_sig_IO bs)
-}
  
do_hash' :: BS -> CVM BS
do_hash' bs = return (doHash bs)

do_start_par_thread :: Loc -> Term -> RawEv -> CVM ()
do_start_par_thread loc t e = undefined

doRemote_session' :: Term -> Plc -> EvC -> CVM EvC
doRemote_session' t pTo e = undefined

do_wait_par_thread :: Loc -> CVM EvC
do_wait_par_thread l = undefined

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


