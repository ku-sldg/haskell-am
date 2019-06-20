{-  Concrete cryptographic operations invoked by Copland interpreters.

  Author: Adam Petz
  Date:  06/14/2019
-}


{-# LANGUAGE OverloadedStrings #-}
module CryptoImpl where

--import System.Environment (lookupEnv)
import qualified Data.ByteString as B
import qualified Crypto.Nonce as CN
--import qualified Data.ByteString.Base64 as B64

import qualified Crypto.Hash.SHA256 as H (hash)
--import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Crypto.Sign.Ed25519

import Crypto.RNG

doRNG :: IO (B.ByteString)
doRNG = do
  rngState <- newCryptoRNGState
  bytes <- randomBytesIO 16 rngState
  return bytes

{-doRNGnum :: IO Int
doRNGnum = do
  val <- randomR (0,1000000)
  return val -}

doNonce :: IO (B.ByteString)
doNonce = do
  g <- CN.new
  n <- CN.nonce128 g
  return n

doHash :: B.ByteString -> B.ByteString
doHash bs = H.hash bs {-convertToBase Base64 (hashWith SHA256 bs)-}

doHashFile :: String -> IO (B.ByteString)
doHashFile s = do
  fileContent <- B.readFile s
  return $ doHash fileContent
                       
doSign :: B.ByteString -> B.ByteString -> IO (B.ByteString)
doSign priKeyBits msg = do
  --sk <- lookupSecretKey
  return $ sign (SecretKey priKeyBits) msg --(unSignature (dsign (SecretKey sk) msg)) {-(B64.encode (unSignature (dsign (SecretKey sk) msg)))-}

test :: IO ()
test = do
  --sk <- lookupSecretKey
  --let pk = toPublicKey (SecretKey sk)
  (_,(SecretKey skBits)) <- createKeypair
  let sk = (SecretKey skBits)

  B.writeFile "bFile.txt" skBits
  bitsRead <- B.readFile "bFile.txt"
  
  let skRead = (SecretKey bitsRead)

  
  let pk' = toPublicKey skRead
  let msg = sign skRead "Hello World"
  let res = (verify pk' msg)
  putStrLn $ "result: " ++ (show res)
