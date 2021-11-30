{-  Executable that acts as the top-level client, performing the first request(s) of an attestation protocol execution.  Sequences execution of Copland phrases, collects results, and optionally performs appraisal.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland
import MonadCop (lookupSecretKeyBytesIO, lookupSecretKeyPath, build_Cop_Env_AM, runCOP, Cop_Env)
import MonadAM
--import ExecCopland
import MonadVM
--import Interp (getNameMap,interp)
import Comm (genNameServer)
import ClientProgArgs (getClientOptions, Client_Options(..))
--import qualified Appraise as APP (appraiseUsm)
import qualified CryptoImpl as CI (doHashFile)
import ServerAppUtil(spawn_the_servers)
import qualified DemoStates as DS (am_env_init, vm_state_init)
import UDcore

import Control.Monad.Trans(liftIO)
import Data.List(union)
import Text.Read(readMaybe)
import Crypto.Sign.Ed25519 (SecretKey(..), verify, toPublicKey)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (threadDelay)
import qualified Data.ByteString as B (empty, writeFile)

import qualified Data.Aeson as DA (encode, FromJSON) --(decodeStrict, encode, FromJSON)
import qualified Network.Socket.ByteString as NBS (recv, sendAll)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified JsonCopland as JC (decodeGen)
import Network.Socket (Socket)
import Numeric.Natural
import Control.Monad(replicateM_)

{-  Receive an attestation response
    Returns:  evidence from response message  -}
getResponse :: DA.FromJSON a => Socket -> IO a
getResponse s = do
  msg <- NBS.recv s 1024
  JC.decodeGen msg
  
sendTestReq :: String -> TestRequestMessage -> IO Natural
sendTestReq sock m = do
  runUnixDomainClient sock (dispatchAt m)
  where dispatchAt m s = do
          let messageBits = DA.encode m
          putStrLn $ "sendAll: " ++ (show messageBits)
          NBS.sendAll s (BL.toStrict messageBits)
          (TestResponseMessage n) <- getResponse s
          return n

oneInc :: IO ()
oneInc = do
  let req = TestIncMessage
  sock <- lookupPath STORE
  n <- sendTestReq sock req
  putStrLn $ "Inc call: " ++ (show n) ++ "\n\n"

oneGet :: IO ()
oneGet = do
  let req = TestGetMessage
  sock <- lookupPath STORE
  n <- sendTestReq sock req
  putStrLn $ "Get call: " ++ (show n) ++ "\n\n"

main :: IO ()
main = do
  replicateM_ 10 oneInc
  oneGet
  
