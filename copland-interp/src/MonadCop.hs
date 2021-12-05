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
      sig_mechanism-  signing mechanism (key or sig server handle)
      me- place ID of currently executing entity
      st_store-  evidence store shared amongst instances
      asp_sockets-  mapping with pointers to asp server socket addresses
-}


data Cop_Env =
  Cop_Env { simulation :: Bool,
            debug :: Bool,
            nameServer :: M.Map Plc Address,
            sig_mechanism :: Sign_Mechanism,
            me :: Plc,
            st_store :: M.Map Natural (TMVar RawEv),
            asp_sockets :: M.Map ASP_ID Address
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
    _ ->
      error "ERROR:  interpreter is not configured with local access to key bytes"
  
getTheirSock :: Plc -> COP Address
getTheirSock pThem = do
  m <- asks nameServer
  let mString = M.lookup pThem m
  case mString of
   Nothing ->
     error "my client error:  server port is not initialized in environment nameserver"
   Just s -> return s

getAspSock :: ASP_ID -> COP Address
getAspSock id = do
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
