{- An experimental Copland protocol term interpreter.  It takes protocol terms
   (plus initial concrete evidence) to concrete evidence values.

   The evaluator operates inside the copland monad (COP).
   COP (defined in MonadCop.hs) facilitates place-specific
   functionality(measurement and crypto) and invokes communication upon encountering an @ term.

   Author: Adam Petz
   Date:  11/08/2018
-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Interp where

import Copland
import CryptoImpl (doNonce, doSign, doHash, doHashFile, doFakeNonce, doFakeSign, doFakeHash, doFakeUsm, doFakeKim)
import Comm
import MonadCop
import qualified ServerProgArgs as SA (Server_Options(..))
--import qualified ExecCopland as EC (run_vm, vm_state_init)

import Control.Monad (forever, void)
import Control.Monad.Trans.Reader(asks, ask, runReaderT)
import Control.Monad.Trans(liftIO)
import Control.Parallel (pseq)
import qualified Control.Concurrent as CC (forkIO, threadDelay)
import qualified Network.Socket as NS (Socket, accept, close)
import qualified Data.Map as M
import qualified Data.Binary as B (decode)
import qualified Data.ByteString.Lazy as BL (fromStrict)


interp' :: ASP -> Ev -> COP Ev
interp' t e =
  case t of
    ASPC i args -> do
      bs <- interpUSM i args
      return $ U i args bs e
    SIG -> do
      bs <- signEv e
      return $ G bs e
    HSH -> do
      bs <- hashEv e
      return $ H bs
    CPY -> return e


{-  Main interp function that interprets protocol terms and initial evidence.
    Params:
      t- protocol term to interpret
      e- initial evidence
    Returns:  resulting evidence  -}
interp :: T -> Ev -> COP Ev
interp t e = do
  --p <- asks me
    case t of
      ASPT a -> interp' a e
  
    {-NONCE -> do
      bs <- genNonce
      --nId <- gets nonceId
      nId <- updateNonce bs
      return $ N p nId bs e -}
      
      AT q t' -> do
      -- TODO: Async logic here?
        (e'{-,c-}) <- toRemote q t' e
      --liftIO $ disconnect c
        return e'
  
      LN t1 t2 -> do
        e1 <- interp t1 e
        logc $ "e1 computed: " ++ (show e1)
        logc $ "by t1: " ++ (show t1)
        logc $ "t2: " ++ (show t2)
        res <- pseq e1 (interp t2 e1)
        return res
  
      BRS (sp1, sp2) t1 t2 -> do
        let es1 = splitEv sp1 e
        let es2 = splitEv sp2 e
        e1 <- interp t1 es1
        e2 <- pseq e1 (interp t2 es2)
        return $ SS e1 e2
      
      BRP (sp1, sp2) t1 t2 -> do
        let es1 = splitEv sp1 e
        let es2 = splitEv sp2 e
        e1 <- interp t1 es1
        e2 <- interp t2 es2
        return $ PP e1 e2

 
{-  Send a remote attestation request and return resulting evidence.
    Invoked upon encountering an AT term during evaluation.
    Params:
      pTo- target place
      t-   protocol term to execute remotely
      e-   initial evidence for remote request
    Returns:  resulting evidence  -}
toRemote :: Pl -> T -> Ev -> COP (Ev{-,Connection-})
toRemote pTo t e = do
  logc "inside toRemote"
  pMe <- asks me
  if (pTo == pMe)
   then do
    logc "sent request to MYSELF!"
    res <- interp t e
    return res
   else do
    mSock <- do
      pString <- getTheirSock pTo
      sock' <- liftIO $ client_resolve_open_localhost pString
      return (sock')

    nameMap <- asks nameServer
    mid <- liftIO $ sendReq mSock pTo pMe nameMap t e
    ResponseMessage _ _ resEv <- liftIO $ receiveResp mSock pTo
    liftIO $ NS.close mSock
    logc $ "Returning evidence result: " ++ (show resEv)
    return resEv

{-
{-  Acts as a server for handling remote attestation requests.
    Params:
      conn- connection handle (Socket) to the client
      opts- server config options provided as command-line arguments

TODO:  move this somewhere besides Interp.hs.
-}
fromRemote :: NS.Socket -> SA.Server_Options -> IO ()
fromRemote conn opts = do
  (RequestMessage pTo pFrom names t e) <- receiveReq conn
  --error $ (show names)
  env <- buildServerEnv opts names pTo

  let compileB = serverCompile opts
  e' <- case compileB of
         True -> do
           let instrs = instr_compiler t
           vm_st <- vm_state_init e
           res <- run_vm instrs vm_st env
           return $ st_ev res
         False -> runCOP (interp t e) env
  
  --e' <- run_interp t e env
  --putStrLn $ "evidence gathered: " ++ (show e')
  sendResp conn pTo pFrom e'
  NS.close conn
-}

{-
{-  Convenience function that runs interp over a Copland term and initial
    evidence, in a specified COP environment, with an empty initial state,
    returning the resulting evidence.  -}
run_interp :: T -> Ev -> Cop_Env -> IO Ev
run_interp t e env = do
  e' <- (runCOP  (interp t e) env)
  return e'
-}

{-  Dispatch function for USM procedures based on ASP_ID  -}
interpUSM :: ASP_ID -> [ARG] -> COP BS
interpUSM i args = do
  sim <- asks simulation
  case sim of
   True -> do
     p <- asks me
     return $ doFakeUsm i p
   False -> 
     case i of
     1 -> if ((length args) == 0)
          then error $ "not enough args to USM: " ++ (show i)
          else do
            let fileName = head args
            {-let fileName_bits = head args
                fileName = B.decode $ BL.fromStrict fileName_bits -}
            
            res <- liftIO $ doHashFile $ "../" ++ fileName
            return res
     _ -> error $ "USM with asp_id not supported: " ++ (show i)        
{-
{-  Dispatch function for KIM procedures based on ASP_ID and target place  -}
interpKIM :: ASP_ID -> Pl -> [ARG] -> COP BS
interpKIM i q args = do
  sim <- asks simulation
  case sim of
   True -> do
     p <- asks me
     return $ doFakeKim i q p
   False -> 
     case i of
     1 ->
       case q of
       1 ->
         if ((length args) == 0)
         then error $ "not enough args to KIM: " ++ (show i)
         else do
           let fileName = head args
           res <- liftIO $ doHashFile $ "../" ++ fileName
           return res
       _ -> error $ "KIM with asp_id " ++ (show i) ++ " at place " ++ (show q) ++ " not supported"
           
     _ -> error $ "KIM with asp_id " ++ (show i) ++ " not supported"
 -} 
signEv :: Ev -> COP BS
signEv ev = do
  simulation <- asks simulation
  sig <- if (simulation)
         then
           do
             p <- asks me
             --let p = fst p'
             return $ doFakeSign p
         else
           do
             let bs = encodeEv ev
             logc $ "encoded Ev before sign: " ++ (show bs)
             keyBits <- lookupSecretKeyBytes
             sig <- liftIO $ doSign keyBits bs
             return sig
  return sig



hashEv :: Ev -> COP BS
hashEv e = do
  simulation <- asks simulation
  h <- if (simulation)
         then
           do
             p <- asks me
             --let p = fst p'
             return $ doFakeHash p
         else
           do
             let bs = encodeEv e
             return $ doHash bs
  return h



genNonce :: COP BS
genNonce = do
  simulation <- asks simulation
  b <- if (simulation)
         then
           do
             p <- asks me
             return $ doFakeNonce p
         else
           do
             b <- liftIO $ doNonce
             return $ b
  return b



splitEv :: SP -> Ev -> Ev
splitEv sp e =
  case sp of
  ALL -> e
  NONE -> Mt

{-
start_standalone_interp_server :: SA.Server_Options -> IO ()
start_standalone_interp_server opts = do
  let pString = SA.server_serverPort opts
  portString <-
    case pString of
     "" -> newTCPPortString
     s -> return s
  
  addr <- resolve portString
  sock <- open addr
  putStrLn $ "starting server" ++ " at port: " ++ portString
  serve_requests sock opts {-void $ CC.forkIO $-} 

serve_requests :: NS.Socket -> SA.Server_Options -> IO ()
serve_requests sock opts = forever $ do
  (conn, peer) <- NS.accept sock
  putStrLn $ "Connection from " ++ show peer
  --error (show opts)
  void $ CC.forkIO $ fromRemote conn opts

spawn_a_server :: Bool -> Bool -> Bool -> Address -> IO ()
spawn_a_server sim debug compile addr = do
  let sopts = SA.Server_Options sim debug addr compile
  void $ CC.forkIO $ start_standalone_interp_server sopts
  
spawn_the_servers :: M.Map Pl Address -> Bool -> Bool -> Bool -> IO ()
spawn_the_servers nm simB debugB compileB = do
  let ps = M.toList nm
      snds = map snd ps
  mapM_ (spawn_a_server simB debugB compileB) snds
-}

getNameMap :: FilePath -> [Pl] -> IO (M.Map Pl String)
getNameMap fileName pls = do
    case fileName of
     "" -> do
       --error "here"
       nm <- genNameServer pls {- TODO: check if this is ok, we will update names upon each remote reuest -}
       --spawn_the_servers nm
       return nm
     _ -> do
       nm <- readNameMap fileName
       --spawn_the_servers nm
       return nm
