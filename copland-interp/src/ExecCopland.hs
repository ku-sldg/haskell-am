{-# LANGUAGE ScopedTypeVariables #-}

module ExecCopland where

--import ServerAppUtil (lookupPath, ServerType(..),vm_state_init)
import CoplandLang
import CoplandInstr
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

build_comp :: Instr -> VM ()
build_comp i = do
  e <- get_ev
  myplace <- lift $ asks me
  case i of
    Copy -> put_ev e
    Umeas i args -> do
                 usmResult <- invokeUSM i args
                 put_ev $ U myplace i args usmResult e
    Sign -> do
           --liftIO $ error "sig body"
           signature <- (signEv e)
           put_ev (G myplace signature e)
    Hash -> put_ev $ H myplace (hashEv e)
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

splitEv sp e =
  case sp of
    ALL -> e
    NONE -> Mt

hashEv :: Ev -> BS
hashEv e =
      doHash $ encodeEv e

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


run_vm :: [Instr] -> VM_St -> Cop_Env -> IO VM_St
run_vm  ilist initState initEnv =
   runReaderT (execStateT (sequence $ map build_comp ilist) initState) initEnv
