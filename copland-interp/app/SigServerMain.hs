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
import ServerAppUtil (startServer, ServerType(SIGN))
import MonadCop (lookupSecretKeyPath,lookupSecretKeyBytesIO)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString as BS (ByteString)
import Network.Socket hiding  (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Map as M
import qualified Data.Aeson as DA (decodeStrict, encode)
import Control.Concurrent
import Crypto.Sign.Ed25519 (dsign, Signature(..), toPublicKey, verify, SecretKey(..))

main :: IO ()
main = do
  startServer SIGN doAt

doAt :: BS.ByteString -> IO BS.ByteString
doAt msg = do
  (SigRequestMessage eBits) <- decodeGen msg

  kp <- lookupSecretKeyPath
  priKeyBits <- lookupSecretKeyBytesIO kp
  let priKey = SecretKey priKeyBits
      sig = dsign priKey eBits
      sigBS = unSignature sig

  
  let sBits = sigBS
  let respMsg = DA.encode (SigResponseMessage sBits)
  return (BL.toStrict respMsg)
