{-# LANGUAGE ScopedTypeVariables #-}

module ExecCopland where

--import ServerAppUtil (lookupPath, ServerType(..),vm_state_init)
import CoplandLang
{-import CoplandInstr -}
import qualified JsonCopland as JC (decodeGen)
import ClientProgArgs
import MonadCop
import MonadAM
import MonadVM
import UDcore

import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader(asks, ask, runReaderT)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Aeson as DA (decodeStrict, encode, FromJSON)
import Numeric.Natural
import CryptoImpl (doNonce, doSign, doHash, doHashFile)
import Crypto.Sign.Ed25519
import Control.Monad.Trans(liftIO)
import Network.Socket
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.Binary as B (decode)
import qualified Data.ByteString.Lazy as BL (fromStrict)

copyEv :: VM Ev
copyEv = get_ev

do_prim :: ASP -> VM Ev
do_prim a =
  case a of
    CPY -> copyEv
    SIG -> do
      e <- get_ev
      bs <- signEv e
      return $ G bs e
    HSH -> do
      e <- get_ev
      bs <- hashEv e
      return $ H bs
    ASPC i args -> do
      e <- get_ev
      bs <- invokeUSM i args
      return $ U i args bs e

put_comm_request :: AnnoTerm -> Pl -> Natural -> Natural -> VM ()
put_comm_request t q reqi rpyi = return ()

poll_comm_response :: Natural -> VM Ev
poll_comm_response rpyi = return Mt 


sendReq :: AnnoTerm -> Pl -> Natural -> Natural -> VM ()
sendReq t q reqi rpyi = do
  {-p <- get_pl-}
  e <- get_ev
  put_store_at reqi e
  put_comm_request t q reqi rpyi

receiveResp :: Natural -> VM Ev
receiveResp rpyi = do
  poll_comm_response rpyi

put_par_request :: AnnoTerm -> (Natural, Natural) -> VM ()
put_par_request t pr@(loc1, loc2) = return ()

poll_par_response :: Natural -> VM Ev
poll_par_response n = return Mt

poll_par_responses :: (Natural,Natural) -> VM (Ev,Ev)
poll_par_responses (n,m) = do
  e1 <- poll_par_response n
  e2 <- poll_par_response m
  return (e1,e2)

runParThread :: AnnoTerm -> (Natural, Natural) -> VM ()
runParThread t pr@(loc1,loc2) = do
  e <- get_store_at loc1
  put_par_request t pr

runParThreads :: AnnoTerm -> AnnoTerm -> (Natural,Natural) -> (Natural,Natural) -> VM ()
runParThreads t1 t2 t1_locs t2_locs = do
  runParThread t1 t1_locs
  runParThread t2 t2_locs
  

build_comp :: AnnoTerm -> VM ()
build_comp t = do
 {- e <- get_ev -}
  --myplace <- lift $ asks me
  case t of
    AASPT _ a -> do
      e <- do_prim a
      put_ev e
    AAT (reqi,rpyi) q t' -> do
      sendReq t' q reqi rpyi
      e' <- receiveResp rpyi
      put_ev e'
    ALN _ t1 t2 -> do
      build_comp t1
      build_comp t2
    ABRS _ (sp1,sp2) t1 t2 -> do
      e <- get_ev
      {-p <- get_pl-}
      let (e1,e2) = splitEvm sp1 sp2 e
      put_ev e1
      build_comp t1
      e1r <- get_ev
      put_ev e2
      build_comp t2
      e2r <- get_ev
      put_ev $ SS e1r e2r
    ABRP (x,y) (sp1,sp2) t1 t2 -> do
      e <- get_ev
      {-p <- get_pl-}
      let (e1,e2) = splitEvm sp1 sp2 e
      let loc_e1  = fst (range t1)
      let loc_e1' = snd (range t1) - 1
      let loc_e2  = fst (range t2)
      let loc_e2' = snd (range t2) - 1
      put_store_at loc_e1 e1
      put_store_at loc_e2 e2
      runParThreads t1 t2 (loc_e1,loc_e1') (loc_e2,loc_e2')
      (e1r,e2r) <- poll_par_responses (loc_e1', loc_e2')
      put_ev $ PP e1r e2r
      
    
      
      
    {-
    Split sp1 sp2 -> do
                 put_ev (splitEv sp1 e)
                 push_stackm (splitEv sp2 e)
    Joins -> do
           er <- pop_stackm
           put_ev $ SS er e
    Besr -> do
           er <- pop_stackm
           push_stackm e
           put_ev er
    {-I'm doing Req/Rpy as a single instruction. -}
    Reqrpy destPlace q -> do
      liftIO $ putStrLn $ "Sending evidence: " ++ (show e)
      e' <- toRemote destPlace q e
      put_ev e'
{- Branch parallel (Bep and Joinp) will be excluded for now.  October 2019
  To perform operations in parallel will require extensive
  extensions to the VM.
-}
    _ -> fail $ "Unrecognied Instruction: " ++ (show i)

-}

splitEv sp e =
  case sp of
    ALL -> e
    NONE -> Mt

splitEvm :: SP -> SP -> Ev -> (Ev,Ev)
splitEvm sp1 sp2 e =
  let e1 = splitEv sp1 e in 
  let e2 = splitEv sp2 e in
    (e1,e2)

hashEv :: Ev -> VM BS
hashEv e =
      return $ doHash $ encodeEv e

signEv :: Ev -> VM BS
signEv e = do
  connectionServerSocket <- get_sigSocket
  --pFrom <- lift $ asks me
  --namesFrom <- lift $ asks nameServer
  liftIO $ runUnixDomainClient connectionServerSocket (dispatchAt)
      where dispatchAt s = do
              let evBits = encodeEv e
              let messageBits = DA.encode (SigRequestMessage evBits)
              NBS.sendAll s (BL.toStrict messageBits)
              (SigResponseMessage sigBits) <- getResponse s
              return sigBits

  {-
  let bs = encodeEv e
  in
    do
      privKeyBits <- lift $ lookupSecretKeyBytes
      return $ bs -- privKeyBits
-}

invokeUSM :: ASP_ID -> [ARG] -> VM BS
invokeUSM asp args = do
  aspSocket <- get_asp_socket asp
  liftIO $ putStrLn $ "aspSocket: " ++ (show aspSocket)

  liftIO $ runUnixDomainClient aspSocket (dispatchAt)
      where dispatchAt s = do
              let aspRequest = (AspRequestMessage args)
                  messageBits = DA.encode aspRequest
              NBS.sendAll s (BL.toStrict messageBits)
              (AspResponseMessage resBits) <- getResponse s
              return resBits

toRemote :: Pl -> T -> Ev -> VM Ev
toRemote pTo q initEvidence = do
  connectionServerSocket <- get_serverSocket  --get Comm Server socket
  pFrom <- lift $ asks me
  namesFrom <- lift $ asks nameServer
  liftIO $ runUnixDomainClient connectionServerSocket
    (dispatchAt pFrom namesFrom)
  where dispatchAt pFrom namesFrom s = do
          let messageBits =
                DA.encode (RequestMessage pTo pFrom namesFrom q initEvidence)
          putStrLn $ "sendAll: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          (ResponseMessage _ _ e') <- getResponse s
          return e'
            
{-  Receive an attestation response
    Returns:  evidence from response message  -}
getResponse :: DA.FromJSON a => Socket -> IO a
getResponse s = do
  msg <- NBS.recv s 1024
  JC.decodeGen msg

{- ************************************************************* -}


run_vm :: AnnoTerm -> VM_St -> Cop_Env -> IO VM_St
run_vm  t initState initEnv =
   runReaderT (execStateT (build_comp t) initState) initEnv
