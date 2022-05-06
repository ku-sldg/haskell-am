{-  Utility functions for socket communication.

  Author: Adam Petz
  Date:  06/014/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module CommUtil where

import Copland
--import qualified ServerProgArgs as SA (Server_Options(..))
--import CommImpl
--import MonadCop
--import MonadAM
import UDcore
import BS (BS)

import Network.Socket as NS hiding (recv)
import Text.Read(readMaybe)
import Control.Exception (bracket)
import Control.Monad(replicateM, mapM_)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (forkIO, threadDelay)

import Control.Concurrent.STM
import qualified Data.Aeson as DA (decodeStrict, encode, ToJSON, FromJSON)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Numeric.Natural
import Control.Monad.Trans(liftIO)


gen_client_session :: BS -> NS.Socket -> IO BS
gen_client_session msg conn = do
  NBS.sendAll conn msg
  NBS.recv conn 2048

gen_run_client ::  (DA.ToJSON a,{-DA.FromJSON a,DA.ToJSON b,-}DA.FromJSON b) =>
                   String -> Address -> a -> IO b
gen_run_client s addr reqm = do
  runUnixDomainClient addr (sendRec' s reqm)

gen_server_async_accept ::( {-DA.ToJSON a, -} DA.FromJSON a) =>
                        String -> (a -> IO ()) -> NS.Socket -> IO ()
gen_server_async_accept s f conn = do
  msg <- NBS.recv conn 2048
  msg_decoded <- decodeGen s msg
  f msg_decoded
  

gen_server_session :: ({-DA.ToJSON a,-} DA.FromJSON a, DA.ToJSON b {-, DA.FromJSON b-}) => String -> 
                      (a -> IO b) -> NS.Socket -> IO ()
gen_server_session s f conn = do
  msg <- NBS.recv conn 2048
  msg_decoded <- decodeGen s msg
  msg' <- f msg_decoded
  let msg'_encoded =  DA.encode msg'
  NBS.sendAll conn (BL.toStrict msg'_encoded)

asyncClientSend :: (DA.ToJSON a) =>
                   a -> NS.Socket -> IO ()
asyncClientSend rm conn = do
  let messageBits = DA.encode rm
      msg = BL.toStrict messageBits
  NBS.sendAll conn msg

sendRec' :: (DA.ToJSON a,{-DA.FromJSON a,DA.ToJSON b,-}DA.FromJSON b) =>
            String -> a -> NS.Socket -> IO b
sendRec' s rm conn = do
  --let msg = sendRec' pTo pFrom namesFrom t e
  let messageBits = DA.encode rm
      msg = BL.toStrict messageBits
  msg' <- gen_client_session msg conn
  decodeGen s msg'
  --res <- decodeGen msg' --(res :: ResponseMessage) <- decodeGen msg'
  --return res

resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr

open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        let fd = fdSocket sock
        setCloseOnExecIfNeeded fd
        listen sock 10
        return sock
        
clientResolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr

clientOpen addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

client_resolve_open_localhost :: Address -> IO Socket
client_resolve_open_localhost portString = do
  addr <- clientResolve "127.0.0.1" portString
  --putStrLn $ show addr
  sock <- clientOpen addr
  return sock



genNameServer :: [Plc] -> IO (M.Map Plc String)
genNameServer pls = do
  ports <- replicateM (length pls) newTCPPortString
  let pairs = zip pls ports
  return (M.fromList pairs)


readNameMap :: FilePath -> IO (M.Map Plc String)
readNameMap fp = do
      s <- readFile fp
      let ss = lines s
          pairs = map readPlacePort ss
      return (M.fromList pairs)

readPlacePort :: String -> (Plc, String)
readPlacePort s =
  let (sNum,sPortString') = break (== ':') s in
  let sPortString = dropWhile (== ' ') (drop 1 sPortString') in
  let (maybePl :: Maybe Plc) = readMaybe sNum in
  case maybePl of
   Nothing -> error "could not parse place number in file"
   Just pl ->
     case sPortString of
      "" -> (pl,sPortString)
      _ -> 
        let (mPortString :: Maybe Int) = readMaybe sPortString in {- TODO: better error handling -}
        case mPortString of
         Nothing -> error "port string did not parse as a number"
         Just _ -> (pl,sPortString)


{- Adapted from:  https://github.com/hargettp/courier/blob/master/tests/TestUtils.hs -}

newTCPPort :: IO NS.PortNumber
newTCPPort = do
  NS.SockAddrInet p _ <- availablePort NS.AF_INET NS.Stream
  return $ p --Name $ "localhost:" ++ show p

newTCPPortString :: IO String
newTCPPortString = do
  p <- newTCPPort
  return (show p)
  
availablePort :: NS.Family -> NS.SocketType -> IO NS.SockAddr
availablePort f t = do
  let hints = NS.defaultHints { NS.addrFamily = f
                              , NS.addrSocketType = t
                              , NS.addrFlags = [ NS.AI_PASSIVE ]
                              , NS.addrProtocol = NS.defaultProtocol }
  addrs <- NS.getAddrInfo (Just hints) Nothing (Just "0")
  let a = head addrs
  bracket
    (NS.socket (NS.addrFamily a) (NS.addrSocketType a) (NS.addrProtocol a))
    NS.close
    (\s -> do
       NS.bind s (NS.addrAddress a)
       addr <- NS.getSocketName s
       if isPrivileged addr
         then availablePort f t
         else return addr
    )

isPrivileged :: NS.SockAddr -> Bool
isPrivileged (NS.SockAddrInet p _) = p < 1025
isPrivileged (NS.SockAddrInet6 p _ _ _) = p < 1025
isPrivileged _ = False

{-
derive_comm_reqs :: AnnoTerm -> M.Map Pl Address -> Pl ->
  IO ([CommReqMessage], M.Map Natural (TMVar Ev))
derive_comm_reqs t nm pFrom = derive_comm_reqs' t nm pFrom ([], M.empty)

derive_comm_reqs' :: AnnoTerm -> M.Map Pl Address -> Pl ->
  ([CommReqMessage], M.Map Natural (TMVar Ev)) ->
  IO ([CommReqMessage], M.Map Natural (TMVar Ev))
derive_comm_reqs' t nm pFrom res@(reqs,store) =
  case t of
    AASPT _ _ -> return res
    AAT (reqi,rpyi) q t' -> do
      reqTMVar <- atomically $ newEmptyTMVar
      rpyTMVar <- atomically $ newEmptyTMVar
      let newMsg = ReqMessage $ CommSetMessage q pFrom nm (unanno t') reqTMVar rpyTMVar
      let reqMap = M.insert reqi reqTMVar store
      let rpyMap = M.insert rpyi rpyTMVar reqMap
      putStrLn $ "inserting reqi index: " ++ (show reqi)
      putStrLn $ "inserting rpyi index: " ++ (show rpyi)
      return (newMsg : reqs, rpyMap)
    ALN _ t1 t2 -> do
      res' <- derive_comm_reqs' t1 nm pFrom res
      derive_comm_reqs' t2 nm pFrom res'
    ABRS _ _ t1 t2 -> do
      res' <- derive_comm_reqs' t1 nm pFrom res
      derive_comm_reqs' t2 nm pFrom res'
    ABRP _ _ t1 t2 -> do
      let loc_e1  = fst (range t1)
          loc_e1' = snd (range t1) + 1--- 1
          loc_e2  = fst (range t2)
          loc_e2' = snd (range t2) + 1 --- 1
      e1_tmvar_init  <- atomically $ newEmptyTMVar
      e1_tmvar_final <- atomically $ newEmptyTMVar
      e2_tmvar_init  <- atomically $ newEmptyTMVar
      e2_tmvar_final <- atomically $ newEmptyTMVar
      let newMsg1 = ParMessage $ CommParMessage pFrom nm (unanno t1) e1_tmvar_init e1_tmvar_final
          newMsg2 = ParMessage $ CommParMessage pFrom nm (unanno t2) e2_tmvar_init e2_tmvar_final
          e1Map  = M.insert loc_e1  e1_tmvar_init store
          e1Map' = M.insert loc_e1' e1_tmvar_final e1Map
          e2Map  = M.insert loc_e2  e2_tmvar_init e1Map'
          e2Map' = M.insert loc_e2' e2_tmvar_final e2Map
      return (newMsg1 : newMsg2 : reqs, e2Map')
-}
                 
      
{-
setupReqOne :: CommReqMessage -> IO ()
setupReqOne m =
  case m of
    ReqMessage m' -> do
      putStrLn "forking a Comm thread\n"
      _ <- CC.forkIO $ setupReqCommOne m'
      --setupReqCommOne m'
      return ()
    ParMessage m' -> do
      putStrLn "forking a Par thread\n"
      _ <- CC.forkIO $ setupReqParOne m'
      --setupReqParOne m'
      return ()
      {-
  _ <- CC.forkIO $ 
    case m of
      ReqMessage m' -> setupReqCommOne m'
      ParMessage m' -> do
        putStrLn "forking a Par thread\n"
        setupReqParOne m'
  return () -}


-}

{-
setupReqParOne :: CommParMessage -> IO ()
setupReqParOne (CommParMessage pTo namesFrom t init_c final_c) = do
  --_ <- CC.forkIO $ do
    parServerSocket <- lookupPath PAR  --get Comm Server socket
    --pFrom <- lift $ asks me
    --namesFrom <- lift $ asks nameServer
    putStrLn $ "Executing par term: " ++ (show t)
    putStrLn $ "Par socket: " ++ parServerSocket

    {-
    let initEvidence = Mt

    resEv <- do
      runUnixDomainClient parServerSocket
        (dispatchAt pTo namesFrom t initEvidence)
    putStrLn $ "Evidence collected: " ++ (show resEv)
-}




    
    initEvidence <- atomically $ takeTMVar init_c
    --error "HIHIHIH"
    resEv <- do
      runUnixDomainClient parServerSocket
        (dispatchAt pTo namesFrom t initEvidence)
    atomically $ putTMVar final_c resEv
  --return ()
  where dispatchAt pTo namesFrom t ev s = do
          let messageBits = DA.encode (RequestMessagePar pTo namesFrom t ev)
          putStrLn $ "sendAllReqParOne: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          (ResponseMessagePar e') <- getResponse s
          return e'
-}

{-
setupReqCommOne :: CommSetMessage -> IO ()
setupReqCommOne (CommSetMessage pTo pFrom namesFrom t init_c final_c) = do
  --_ <- CC.forkIO $ do
    connectionServerSocket <- lookupPath COMM  --get Comm Server socket
    --pFrom <- lift $ asks me
    --namesFrom <- lift $ asks nameServer
    initEvidence <- atomically $ takeTMVar init_c
    putStrLn $ "Got initial evidence for setupReqCommOne: " ++ (show initEvidence)
    resEv <- do
      runUnixDomainClient connectionServerSocket
        (dispatchAt pTo pFrom namesFrom t initEvidence)
    atomically $ putTMVar final_c resEv
  --return ()
  where dispatchAt pTo pFrom namesFrom t ev s = do
          let messageBits = DA.encode (RequestMessage pTo pFrom namesFrom t ev)
          putStrLn $ "sendAllReqCommOne: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          resp@(ResponseMessage _ _ e') <- getResponse s
          putStrLn $ "receivedReqCommOne: " ++ (show resp )
          return e'

setupComm :: [CommReqMessage] -> IO ()
setupComm ls = mapM_ setupReqOne ls
-}

{-
  do
  connectionServerSocket <- lookupPath COMM --get_serverSocket  --get Comm Server socket
  --pFrom <- lift $ asks me
  --namesFrom <- lift $ asks nameServer
  liftIO $ runUnixDomainClient connectionServerSocket
    (dispatchAt {-pFrom namesFrom-})
  where dispatchAt {-pFrom namesFrom -} s = do
          let messageBits = DA.encode (CommSetList ls)
          putStrLn $ "sendAll: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          CommAckMessage <- getResponse s
          return ()
-}

{-  Receive an attestation response
    Returns:  evidence from response message  -}
getResponse :: DA.FromJSON a => String -> Socket -> IO a
getResponse s sock = do
  msg <- NBS.recv sock 2048
  putStrLn $ "Raw received in getResponse: " ++ (show msg)
  decodeGen s msg
