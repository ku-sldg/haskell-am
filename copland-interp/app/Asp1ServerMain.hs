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
import ServerAppUtil (startServer, ServerType(ASP_SERV))
import CryptoImpl (doHashFile)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Control.Concurrent
import qualified Data.Binary as B (decode,encode)
import qualified Data.ByteString.Lazy as BL (fromStrict)

main :: IO ()
main = do
  startServer (ASP_SERV 1) doAt

{-
invokeUSM :: ASP_ID -> [ARG] -> VM BS
invokeUSM asp args = do
  case asp of
    1 -> if ((length args) == 0)
         then error $ "not enough args to USM: " ++ (show asp)
         else do
           let fileName_bits = head args
               fileName = B.decode $ BL.fromStrict fileName_bits
           
           liftIO $ doHashFile $ "../" ++ fileName
    _ -> error $ "USM with asp_id not supported: " ++ (show asp)
-}

doAt :: BS.ByteString -> IO BS.ByteString
doAt msg = do
  error "herrrre"
  (AspRequestMessage args) <- decodeGen msg
  --error "here"
  if ((length args) == 0)
         then error $ "not enough args to ASP."
         else do
           let fileName_bits = head args
               fileName = B.decode $ BL.fromStrict fileName_bits
           error fileName
           
           hashBits <- doHashFile $ "../" ++ fileName
           let respMsg = DA.encode (AspResponseMessage hashBits)
           return $ BL.toStrict respMsg



  {-
  let sBits = eBits -- TODO sig algorithm here
  let respMsg = DA.encode (SigResponseMessage sBits)
  return (BL.toStrict respMsg)-}
