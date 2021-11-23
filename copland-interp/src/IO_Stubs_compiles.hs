module IO_Stubs where

--import Copland
import CoplandLang_Extracted(ASP_PARAMS(..), RawEv, BS, Plc, EvC(..), Term, Loc)
import CryptoImpl (doHash, doSign)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B (ByteString, readFile, concat)

encodeEvRaw :: RawEv -> BS
encodeEvRaw e = B.concat e

do_asp :: ASP_PARAMS -> BS
do_asp params = undefined


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

do_sig_IO :: BS -> IO BS
do_sig_IO bs = do
  key_bits <- get_key_simpl
  doSign key_bits bs

do_sig :: BS -> BS
do_sig bs = unsafePerformIO (do_sig_IO bs)
  
do_hash :: BS -> BS
do_hash = doHash

do_start_par_threadIO :: Loc -> Term -> RawEv -> IO ()
do_start_par_threadIO loc t e = undefined

doRemote_session :: Term -> Plc -> EvC -> EvC
doRemote_session t pTo e = undefined

parallel_vm_thread :: Loc -> EvC
parallel_vm_thread l = undefined


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


