{-  An experimental monad library for managing Copland phrase execution.  Requires a read-only environment and IO.

   Author: Adam Petz
   Date:  11/08/2018
-}
--{-# LANGUAGE ScopedTypeVariables #-}

module MonadCop where

import Copland
import qualified ServerProgArgs as SA
import ClientProgArgs (getClientOptions, Client_Options(..))
import CryptoImpl (lookupSecretKeyBytesIO, lookupSecretKeyPath)
import CommTypes(Sign_Mechanism)

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
--import System.Environment (lookupEnv)
import qualified Data.ByteString as B (ByteString, readFile)
import qualified Data.Map as M

import Control.Concurrent.STM
import Numeric.Natural

{-  The Copland Monad  -}
type COP = ReaderT Cop_Env IO

{-  Read-only environment used during COPLAND phrase execution.
    Contains:
      simulation- true for real crypto operations, false for simulated
      debug- true for verbose debug output
      nameServer-  mapping from place ID to concrete address
      myKeyPath-  fully qualified filePath for this place's private key bits
      me- place ID of currently executing entity
-}


data Cop_Env =
  Cop_Env { simulation :: Bool,
            debug :: Bool,
            nameServer :: M.Map Plc Address,
            sig_mechanism :: Sign_Mechanism,
            {-myKeyPath :: FilePath,
            sig_socket :: Address, -}
            me :: Plc,
            st_store :: M.Map Natural (TMVar RawEv),
            asp_sockets :: M.Map ASP_ID String
          }


put_store_at :: Natural -> RawEv -> COP ()
put_store_at n e = do
  {-st <- get
  let store = st_store st -}
  store <- asks st_store
  let maybeVar = M.lookup n store
  case maybeVar of
    Just v -> do
      --let store' = M.insert n e store
      liftIO $ atomically $ putTMVar v e 
      --put (st {st_store = store'})
    Nothing -> error $ "st_store not configured with entry at index: " ++ (show n) ++ "."

get_store_at :: Natural -> COP RawEv
get_store_at n = do
  store <- asks st_store
  let maybeVar = M.lookup n store
  case maybeVar of
    Just v -> do
      e <- liftIO $ atomically $ takeTMVar v
      return e  
    Nothing -> error $ "st_store not configured with entry at index: " ++ (show n) ++ "."

lookupSecretKeyBytes :: COP B.ByteString
lookupSecretKeyBytes = do
  sm <- asks sig_mechanism
  case sm of
    Sign_Keypath fp ->
      liftIO $ lookupSecretKeyBytesIO fp
    _ -> error "ERROR:  interpreter is not configured with local access to key bytes"

      {-
  fp <- asks myKeyPath
  liftIO $ lookupSecretKeyBytesIO fp
  -}
  {-
  bs <- liftIO $ B.readFile fp
  return bs -}

{-
--TODO: refactor this IO function after moving appraisal into the COP monad, or after more general public key management.
lookupSecretKeyBytesIO :: FilePath -> IO B.ByteString
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs
-}
  
getTheirSock :: Plc -> COP Address
getTheirSock pThem = do
  m <- asks nameServer
  let mString = M.lookup pThem m
  case mString of
   Nothing -> error "my client error:  server port is not initialized in environment nameserver"
   Just s -> return s

{-
getMySock :: Cop_Env -> IO Address
getMySock env = do
  let p = me env
  let m = nameServer env
  let mString = M.lookup p m
  case mString of
   Nothing -> error "server port is not initialized in environment nameserver"
   Just s -> return s
-}


getAspSock :: ASP_ID -> COP Address
getAspSock id = do
  {-let p = me env
  let m = nameServer env -}
  aspmap <- asks asp_sockets
  let mString = M.lookup id aspmap
  case mString of
   Nothing -> error "asp port is not initialized in COP environment"
   Just s -> return s

--Debug-dependent output
logc :: String -> COP ()
logc s = do
  b <- asks MonadCop.debug
  if (b)
    then liftIO $ putStrLn s
    else return ()

runCOP :: COP a -> Cop_Env -> IO a
runCOP k env =
     runReaderT k env


{-
build_Cop_Env_Client ::
  SA.Server_Options -> M.Map Plc Address -> Plc ->
  M.Map Natural (TMVar RawEv) -> M.Map ASP_ID String -> Address ->
  IO Cop_Env
build_Cop_Env_Client opts nameMap pl store aspMap sigSock = do

  let b = SA.server_optSim opts
      d = SA.server_optDebug opts
      --pl = 0 -- TODO:  hardcoded
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath sigSock pl store aspMap
  {- TODO: ok to return place 0, since it will be updated? -}
-}

{-
build_Cop_Env_AM ::
  Client_Options -> M.Map Plc Address ->
  M.Map Natural (TMVar RawEv) -> M.Map ASP_ID String ->
  IO Cop_Env
build_Cop_Env_AM opts nameMap store aspMap = do

  let b = optSim opts
      d = optDebug opts
      pl = 0 -- TODO:  hardcoded
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath pl store aspMap
  {- TODO: ok to return place 0, since it will be updated? -}
-}

{-
buildServerEnv :: Bool -> Bool -> 
  {-SA.Server_Options ->-} M.Map Plc Address -> Plc ->
  M.Map Natural (TMVar RawEv) -> M.Map ASP_ID String -> Address ->
  IO Cop_Env
buildServerEnv b d nameMap myPlace store aspMap sigSock = do

  {-
  let b = SA.server_optSim opts
      d = SA.server_optDebug opts
      pl = myPlace -- TODO:  Do we even need this in Cop_Env??
      -- TODO:  sanity check that myPlace is in nameMap
  -}

  --print "HERE"
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath sigSock myPlace store aspMap
-}


{-
build_Cop_Env :: Bool -> Bool -> 
  {-SA.Server_Options ->-} M.Map Plc Address -> Plc ->
  M.Map Natural (TMVar RawEv) -> M.Map ASP_ID String -> Address ->
  IO Cop_Env
build_Cop_Env b d nameMap myPlace store aspMap sigSock = do

  {-
  let b = SA.server_optSim opts
      d = SA.server_optDebug opts
      pl = myPlace -- TODO:  Do we even need this in Cop_Env??
      -- TODO:  sanity check that myPlace is in nameMap
  -}

  --print "HERE"
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath sigSock myPlace store aspMap
-}

