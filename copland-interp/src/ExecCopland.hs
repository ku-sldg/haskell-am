{-# LANGUAGE ScopedTypeVariables #-}

module ExecCopland where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader(asks, ask, runReaderT)
import System.Environment (lookupEnv)

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Aeson as DA (decodeStrict, encode)
import Numeric.Natural
import CryptoImpl (doNonce, doSign, doHash, doHashFile)
import Crypto.Sign.Ed25519
import Control.Monad.Trans(liftIO)

import CoplandLang
import CoplandInstr
import ClientProgArgs
import MonadCop
import MonadAM
import MonadVM
import UDcore
import Network.Socket
import qualified Network.Socket.ByteString as NBS (recv, sendAll)

build_comp :: Instr -> VM ()
build_comp i = do
  e <- get_ev
  myplace <- lift $ asks me
  case i of
    Copy -> put_ev e
    Kmeas i q args -> do
                 kimResult <- invokeKIM i q args
                 put_ev $ K i args myplace q kimResult  e
    Umeas i args -> do
                 usmResult <- invokeUSM i args
                 put_ev $ U i args myplace usmResult e
    Sign -> do
           signature <- (signEv e)
           put_ev (G myplace e signature)
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
signEv e =
  let bs = encodeEv e
  in
    do
      privKeyBits <- lift $ lookupSecretKeyBytes
      return $ bs -- privKeyBits

invokeUSM :: ASP_ID -> [ARG] -> VM BS
invokeUSM asp args = do
  case asp of
    1 -> if ((length args) == 0)
         then error $ "not enough args to USM: " ++ (show asp)
         else do
           let fileName = head args
           liftIO $ doHashFile $ "../" ++ fileName
    _ -> error $ "USM with asp_id not supported: " ++ (show asp)

invokeKIM :: ASP_ID -> Pl -> [ARG] -> VM BS
invokeKIM asp q args = do
  case asp of
    1 ->
        case q of
          1 ->
            if ((length args) == 0)
            then error $ "not enough args to KIM: " ++ (show asp)
            else do
              let fileName = head args
              liftIO $ doHashFile $ "../" ++ fileName
          _ -> fail $ "KIM with asp_id " ++ (show asp) ++ " at place " ++ (show q) ++ " not supported"

    _ -> fail $ "KIM with asp_id " ++ (show asp) ++ " not supported"


toRemote :: Pl -> T -> Ev -> VM Ev
toRemote pTo q initEvidence = do
  connectionServerSocket <- get_serverSocket
  pFrom <- lift $ asks me
  namesFrom <- lift $ asks nameServer
  liftIO $ runUnixDomainClient connectionServerSocket (dispatchAt pFrom namesFrom)
      where dispatchAt pFrom namesFrom s = do
              let messageBits = DA.encode (RequestMessage pTo pFrom namesFrom q initEvidence)
              NBS.sendAll s (BL.toStrict messageBits)
              (ResponseMessage _ _ e') <- getCommResp s
              return e'

{-  Receive an attestation response
    Returns:  evidence from response message  -}
getCommResp :: Socket -> IO ResponseMessage
getCommResp s = do
  msg <- NBS.recv s 1024
  let (val :: Maybe ResponseMessage) = DA.decodeStrict msg
  case val of
      Nothing -> error $ "weird message received: " ++ (show msg)
      Just res -> do
        return res

lookupUDsocketPath :: IO FilePath
lookupUDsocketPath = do
  maybeBuildPath <- lookupEnv "COPLAND_BUILD" -- TODO: fix hardcoding
  maybeSocketPath  <- lookupEnv "COPLAND_UD_SOCKET"
  socketPath <-
        case maybeSocketPath of
        Just p -> return p
        Nothing ->
          case maybeBuildPath of
           Just s -> do
             return $ s ++ "UDS"
           Nothing ->
             error "Missing both COPLAND_BUILD(for default path) and COPLAND_UD_SOCKET(for custom path) environment variables.  Must have one or the other to connect to the ConnectionServer."
  return socketPath

{- ************************************************************* -}

run_vm :: [Instr] -> Vm_st -> Cop_Env -> IO Vm_st
run_vm  ilist initState initEnv =
   runReaderT (execStateT (sequence $ map build_comp ilist) initState) initEnv

run_vm_t ::  T -> Ev -> M.Map Pl Address  -> IO (Ev)
run_vm_t t e m = do
  opts <- liftIO $ getClientOptions
  cop_env <- liftIO $ build_AM_Env opts m
  udSocketPath <- lookupUDsocketPath
  res <- liftIO $ run_vm (instr_compiler t) (initialState e udSocketPath) cop_env
  return $ st_ev res
