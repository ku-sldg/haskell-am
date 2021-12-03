{-  Concrete cryptographic operations invoked by Copland interpreters.

  Author: Adam Petz
  Date:  06/14/2019
-}


{-# LANGUAGE OverloadedStrings #-}
module CryptoImpl where

import BS(BS)


import Crypto.Sign.Ed25519 (createKeypair, sign, toPublicKey, verify, SecretKey(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CH (pack)
import qualified Crypto.Nonce as CN
import qualified Crypto.Hash.SHA256 as H (hash)
import System.Environment (lookupEnv)

doNonce :: IO (B.ByteString)
doNonce = do
  g <- CN.new
  n <- CN.nonce128 g
  return n

doHash :: B.ByteString -> B.ByteString
doHash bs = H.hash bs

doHashFile :: String -> IO (B.ByteString)
doHashFile s = do
  fileContent <- B.readFile s
  return $ doHash fileContent
                       
doSign :: B.ByteString -> B.ByteString -> IO (B.ByteString)
doSign priKeyBits msg = do
  --sk <- lookupSecretKey
  return $ sign (SecretKey priKeyBits) msg --(unSignature (dsign (SecretKey sk) msg)) {-(B64.encode (unSignature (dsign (SecretKey sk) msg)))-}


doFakeUsm i p = CH.pack $ "u" ++ (show i) ++ "at" ++ (show p)
doFakeKim i q p = CH.pack $ "k" ++ (show i) ++ "_" ++ (show q) ++ "at" ++ (show p)
doFakeSign p = (CH.pack ((show p) ++ "sig"))
doFakeHash p = (CH.pack ((show p) ++ "hash"))
doFakeNonce p = (CH.pack ((show p) ++ "nonce"))

lookupSecretKeyBytesIO :: FilePath -> IO BS
lookupSecretKeyBytesIO fp = do
  --fp <- asks myKeyPath
  bs <- B.readFile fp
  return bs


get_key_simpl :: IO BS
get_key_simpl = do
  --kp <- lookupSecretKeyPath
  let kp = "./key0.txt" in
    lookupSecretKeyBytesIO kp

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
