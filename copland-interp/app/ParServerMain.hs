{-  Executable that activates the ParServer.
    Any attestation process requires that a ParServer
    already be active on the machine.

  Author: Adam Petz, tweaked from ConnectionServerMain.hs by Ed Komp
  Date:  12/20/2020
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Copland
import CommUtil
import UDcore
--import ExecCopland
import ServerAppUtil (startServer)

import qualified DemoStates as DS (vm_state_init)
import MonadVM_Old (st_ev)
import MonadCop(Cop_Env(..), lookupSecretKeyPath)



import Control.Concurrent

import qualified Network.Socket as NS hiding  (recv, sendAll)
import qualified Network.Socket.ByteString as NBS
import Numeric.Natural
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BL (toStrict)


main :: IO ()
main = do
  startServer PAR doAt

doAt msg = do
  req@(RequestMessagePar _ _ _ _) <- decodeGen msg
  resp <- fromRemotePar req
  let messageBits = DA.encode resp
  {-logc $ "sending doSendResp: " ++ (show rm)
  logc $ "JSON Response: " ++ (show messageBits) -}
  --putStrLn $ "JSON Response: " ++ (show messageBits)
  --NBS.sendAll conn (BL.toStrict messageBits)
  return (BL.toStrict messageBits)





  {-
  (RequestMessage pTo pFrom names t e) <- decodeGen msg --decodeRequest msg
  tcpSock <- getTheirSock pTo names
  sendAll tcpSock msg
  respMsg <- recv tcpSock 1024

  -- TODO: decode here?  Or only on client side (since all we do is forward message)?
  let (val :: Maybe ResponseMessage) = DA.decodeStrict respMsg
  case val of
   Nothing -> error $ "weird message received: " ++ (show respMsg)
   Just res ->
     return respMsg
  -}
  


buildServerEnvPar :: {-SA.Server_Options ->-} Bool -> Bool -> M.Map Plc Address -> Plc ->
  M.Map Natural (TMVar EvidenceC) -> IO Cop_Env
buildServerEnvPar {-opts-} b d nameMap myPlace store = do

  {-
  let b = SA.server_optSim opts
      d = SA.server_optDebug opts
      pl = myPlace -- TODO:  Do we even need this in Cop_Env??
-}
      -- TODO:  sanity check that myPlace is in nameMap
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath myPlace store


receiveReqPar :: NS.Socket -> IO RequestMessagePar
receiveReqPar conn = do
  --logc "inside doRecieveRequest"

  msg <-  NBS.recv conn 2048
  let (val :: Maybe RequestMessagePar) = DA.decodeStrict msg
  case val of
   Nothing -> error $ "weird message received: " ++ (show msg)
   Just res -> return res

sendRespPar :: NS.Socket -> {-Pl -> Pl ->-} EvidenceC -> IO ()
sendRespPar conn {-pFrom pTo-} e = do
  --pFrom <- asks me
  let rm = (ResponseMessagePar {-pTo pFrom-} e)
  let messageBits = DA.encode rm
  {-logc $ "sending doSendResp: " ++ (show rm)
  logc $ "JSON Response: " ++ (show messageBits) -}
  --putStrLn $ "JSON Response: " ++ (show messageBits)
  NBS.sendAll conn (BL.toStrict messageBits)

fromRemotePar :: RequestMessagePar -> {-NS.Socket ->-} {-SA.Server_Options ->-} IO ResponseMessagePar
fromRemotePar rreq@(RequestMessagePar pTo names t e) {-conn-} {-opts-} = do
  --rreq@(RequestMessagePar pTo names t e) <- receiveReqPar conn
  --error $ (show names)

  putStrLn $ "Par Req received: " ++ (show rreq) ++ "\n"

  (reqs,store) <- undefined --derive_comm_reqs (annotated t) names pTo -- TODO: Check pTo here
  setupComm reqs

  let sim_bool = False
      debug_bool = False
      
  env <- buildServerEnvPar {-opts-} sim_bool debug_bool names pTo store

  

  {-let compileB = SA.server_compile opts
  e' <- case compileB of
         True -> do -}
           {-let instrs = instr_compiler t -}

  -- TODO: give option of non-compiled?
  vm_st <- DS.vm_state_init e
  putStrLn $ "HHHEEERE_PAR"
  res <- undefined --run_vm (annotated t) vm_st env
  let e' = st_ev res
        {- False -> do
           putStrLn $ "HERE"
           runCOP (interp t e) env 0 -}
  
  --e' <- run_interp t e env
  putStrLn $ "evidence gathered: " ++ (show e')
  return (ResponseMessagePar e')
  --sendRespPar conn e'
  --NS.close conn






getTheirSock :: Plc -> M.Map Plc Address -> IO Socket
getTheirSock pThem nameServer = do
  let mString = M.lookup pThem nameServer
  case mString of
    Nothing -> error "my client error:  server port is not initialized in environment nameserver"
    Just portString -> do
                 addr <- clientResolve "127.0.0.1" portString
                 clientOpen addr
