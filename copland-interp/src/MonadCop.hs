{-  An experimental monad library for managing Copland phrase execution.  Requires a read-only environment and IO.

   Author: Adam Petz
   Date:  11/08/2018
-}
--{-# LANGUAGE ScopedTypeVariables #-}

module MonadCop where

import Copland
import qualified ServerProgArgs as SA
import ClientProgArgs (getClientOptions, Client_Options(..))

import Control.Monad.Reader
import System.Environment (lookupEnv)
import qualified Data.ByteString as B (ByteString, readFile)
import qualified Data.Map as M

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
            nameServer :: M.Map Pl Address,
            myKeyPath :: FilePath,
            me :: Pl
          }

lookupSecretKeyBytes :: COP B.ByteString
lookupSecretKeyBytes = do
  fp <- asks myKeyPath
  bs <- liftIO $ B.readFile fp
  return bs

--TODO: refactor this IO function after moving appraisal into the COP monad, or after more general public key management.
lookupSecretKeyBytesIO :: FilePath -> IO B.ByteString
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs
  
getTheirSock :: Pl -> COP Address
getTheirSock pThem = do
  m <- asks nameServer
  let mString = M.lookup pThem m
  case mString of
   Nothing -> error "my client error:  server port is not initialized in environment nameserver"
   Just s -> return s

getMySock :: Cop_Env -> IO Address
getMySock env = do
  let p = me env
  let m = nameServer env
  let mString = M.lookup p m
  case mString of
   Nothing -> error "server port is not initialized in environment nameserver"
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

build_Cop_Env :: Client_Options -> M.Map Pl Address -> IO Cop_Env
build_Cop_Env opts nameMap = do

  let b = optSim opts
      d = optDebug opts
      pl = 0 -- TODO:  hardcoded
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath pl
  {- TODO: ok to return place 0, since it will be updated? -}

buildServerEnv :: SA.Server_Options -> M.Map Pl Address -> Pl -> IO Cop_Env
buildServerEnv opts nameMap myPlace = do

  let b = SA.server_optSim opts
      d = SA.server_optDebug opts
      pl = myPlace -- TODO:  Do we even need this in Cop_Env??
      -- TODO:  sanity check that myPlace is in nameMap
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath pl

lookupSecretKeyPath :: IO FilePath
lookupSecretKeyPath = do
  maybeBuildPath <- lookupEnv "COPLAND_BUILD" -- TODO: fix hardcoding
  maybeKeysPath  <- lookupEnv "COPLAND_KEY"
  keyPath <-
        case maybeKeysPath of
        Just kp -> return kp
        Nothing ->
          case maybeBuildPath of
           Just s -> do
             return $ s ++ "/keys/key0.txt"
           Nothing ->
             error "Missing both COPLAND_BUILD(for default key) and COPLAND_KEY(for custom key) environment variables.  Must have one or the other to identify a signing key."

  return keyPath
