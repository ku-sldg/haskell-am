{-  Executable that activates the SigServer.
    Any attestation process requires that a (single) SigServer
    already be active on the machine.

  Original Author: Ed Komp
  Date:  12/4/2019

  Adapted by:  Adam Petz
  Date:  4/1/2020
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland
import CommUtil
import UDcore
import ServerAppUtil (startServer)
import CryptoImpl (doHashFile)
import MonadCop (lookupSecretKeyPath,lookupSecretKeyBytesIO)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString as BS (ByteString, empty, readFile)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode, decode)
import Control.Concurrent
import qualified Data.Binary as B (decode,encode,decodeOrFail)
import qualified Data.Binary.Get as BG (ByteOffset)
import qualified Data.ByteString.Lazy as BL (fromStrict,ByteString)
import Crypto.Sign.Ed25519 (Signature(..), toPublicKey, dverify, SecretKey(..))

main :: IO ()
main = do
  putStrLn "starting asp41 server......"
  startServer (ASP_SERV 41) doAt

type GoldenUsmMap = M.Map (ASP_ID,Plc,[ARG]) BS

addGoldenUsm :: ASP_ID -> Plc -> [ARG] -> BS -> GoldenUsmMap -> GoldenUsmMap
addGoldenUsm i p args bs m =
  M.insert (i,p,args) bs m
  
goldenUsms :: IO GoldenUsmMap
goldenUsms = do
  uBits <- BS.readFile "goldenInputBits.txt"
  let u10 = addGoldenUsm 1 0 ["target.txt"] uBits M.empty
  return u10

getGoldenUsm :: ASP_ID -> Plc -> [ARG] -> IO BS
getGoldenUsm i p args = do
  golden <- goldenUsms
  let maybeBS = M.lookup (i,p,args) golden
  case maybeBS of
   Nothing -> error $ "No golden value for USM with asp_id " ++ (show i) ++ " at place " ++ (show p)
   Just bs -> return bs

appraiseUsm :: ASP_ID -> Plc -> [ARG] -> BS -> IO (Bool,BS)
appraiseUsm i p args bs = do
  goldenVal <- getGoldenUsm i p args
  return ((bs == goldenVal),goldenVal)

doAt :: BS.ByteString -> IO BS.ByteString
doAt msg = do
  putStrLn "doAt"
  --error "herree"
  (AspRequestMessage args) <- decodeGen msg
  putStrLn $ "args: " ++ (show args)
  if ((length args) < 2)
         then do
              putStrLn $ "not enough args to ASP."
              return BS.empty
         else do
           let evBitsString = head args
               sigBitsString = head (tail args)
           putStrLn $ "evBits: " ++ evBitsString
           putStrLn $ "sigBits: " ++ sigBitsString

           let (evBits::BS) = read evBitsString
               (sigBits::BS) = read sigBitsString
          
           kp <- lookupSecretKeyPath
           priKeyBits <- lookupSecretKeyBytesIO kp
           let priKey = SecretKey priKeyBits
               pubKey = toPublicKey priKey
               sigResult = dverify pubKey evBits (Signature sigBits)
          
           let uBool = sigResult 
               uBoolBits = B.encode uBool
        
           let respMsg = DA.encode (AspResponseMessage (BL.toStrict uBoolBits))
           putStrLn $ "respMgs: " ++ (show respMsg)
           return $ BL.toStrict respMsg
