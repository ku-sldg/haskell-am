{-  Utility functions for socket communication.

  Author: Adam Petz
  Date:  06/014/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module CommUtil where

import Copland
import qualified ServerProgArgs as SA (Server_Options(..))
--import CommImpl
import MonadCop
import MonadAM
import UDcore

import Network.Socket as NS hiding (recv)
import Text.Read(readMaybe)
import Control.Exception (bracket)
import Control.Monad(replicateM, mapM_)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (forkIO, threadDelay)

import Control.Concurrent.STM
import qualified Data.Aeson as DA (decodeStrict, encode, FromJSON)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Numeric.Natural
import Control.Monad.Trans(liftIO)

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



genNameServer :: [Pl] -> IO (M.Map Pl String)
genNameServer pls = do
  ports <- replicateM (length pls) newTCPPortString
  let pairs = zip pls ports
  return (M.fromList pairs)


readNameMap :: FilePath -> IO (M.Map Pl String)
readNameMap fp = do
      s <- readFile fp
      let ss = lines s
          pairs = map readPlacePort ss
      return (M.fromList pairs)

readPlacePort :: String -> (Pl, String)
readPlacePort s =
  let (sNum,sPortString') = break (== ':') s in
  let sPortString = dropWhile (== ' ') (drop 1 sPortString') in
  let (maybePl :: Maybe Pl) = readMaybe sNum in
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


derive_comm_reqs :: AnnoTerm -> M.Map Pl Address -> Pl ->
  IO ([CommSetMessage], M.Map Natural (TMVar Ev))
derive_comm_reqs t nm pFrom = derive_comm_reqs' t nm pFrom ([], M.empty)

derive_comm_reqs' :: AnnoTerm -> M.Map Pl Address -> Pl ->
  ([CommSetMessage], M.Map Natural (TMVar Ev)) ->
  IO ([CommSetMessage], M.Map Natural (TMVar Ev))
derive_comm_reqs' t nm pFrom res@(reqs,store) =
  case t of
    AASPT _ _ -> return res
    AAT (reqi,rpyi) q t' -> do
      reqTMVar <- atomically $ newEmptyTMVar
      rpyTMVar <- atomically $ newEmptyTMVar
      let newMsg = CommSetMessage q pFrom nm (unanno t') reqTMVar rpyTMVar
      let reqMap = M.insert reqi reqTMVar store
      let rpyMap = M.insert rpyi rpyTMVar reqMap
      return (newMsg : reqs, rpyMap)

setupCommOne :: CommSetMessage -> IO ()
setupCommOne (CommSetMessage pTo pFrom namesFrom t init_c final_c) = do
  _ <- CC.forkIO $ do
    connectionServerSocket <- lookupPath COMM  --get Comm Server socket
    --pFrom <- lift $ asks me
    --namesFrom <- lift $ asks nameServer
    initEvidence <- atomically $ takeTMVar init_c
    resEv <- do
      runUnixDomainClient connectionServerSocket
        (dispatchAt pTo pFrom namesFrom t initEvidence)
    atomically $ putTMVar final_c resEv
  return ()
  where dispatchAt pTo pFrom namesFrom t ev s = do
          let messageBits = DA.encode (RequestMessage pTo pFrom namesFrom t ev)
          putStrLn $ "sendAll: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          (ResponseMessage _ _ e') <- getResponse s
          return e'

setupComm :: [CommSetMessage] -> IO ()
setupComm ls = mapM_ setupCommOne ls

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
getResponse :: DA.FromJSON a => Socket -> IO a
getResponse s = do
  msg <- NBS.recv s 1024
  decodeGen msg
