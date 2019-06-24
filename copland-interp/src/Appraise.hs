--{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Appraise where

import Copland

import qualified Data.ByteString as B
import qualified Data.Map as M
import Crypto.Sign.Ed25519 (SecretKey(..), toPublicKey, verify)

type GoldenUsmMap = M.Map (ASP_ID,Pl,[ARG]) BS
type GoldenKimMap = M.Map (ASP_ID,Pl,[ARG]) BS

goldenUsms :: IO GoldenUsmMap
goldenUsms = do
  uBits <- B.readFile "goldenInputBits.txt"
  let u11 = addGoldenUsm 1 1 ["target.txt"] uBits M.empty
  return u11
  
goldenKims :: IO GoldenKimMap
goldenKims = do
  uBits <- B.readFile "goldenKimBits.txt"
  let u11 = addGoldenKim 1 1 ["kimTarget.txt"] uBits M.empty
  return u11

addGoldenUsm :: ASP_ID -> Pl -> [ARG] -> BS -> GoldenUsmMap -> GoldenUsmMap
addGoldenUsm i p args bs m =
  M.insert (i,p,args) bs m

addGoldenKim :: ASP_ID -> Pl -> [ARG] -> BS -> GoldenKimMap -> GoldenKimMap
addGoldenKim i p args bs m =
  M.insert (i,p,args) bs m

getGoldenUsm :: ASP_ID -> Pl -> [ARG] -> IO BS
getGoldenUsm i p args = do
  golden <- goldenUsms
  let maybeBS = M.lookup (i,p,args) golden
  case maybeBS of
   Nothing -> error $ "No golden value for USM with asp_id " ++ (show i) ++ " at place " ++ (show p)
   Just bs -> return bs

getGoldenKim :: ASP_ID -> Pl -> [ARG] -> IO BS
getGoldenKim i p args = do
  golden <- goldenKims
  let bs = M.findWithDefault B.empty (i,p,args) golden
  return bs

appraiseUsm :: ASP_ID -> Pl -> [ARG] -> BS -> IO (Bool,BS)
appraiseUsm i p args bs = do
  goldenVal <- getGoldenUsm i p args
  return ((bs == goldenVal),goldenVal)

appraiseKim :: ASP_ID -> Pl -> [ARG] -> BS -> IO (Bool,BS)
appraiseKim i p args bs = do
  goldenVal <- getGoldenKim i p args
  return ((bs == goldenVal),goldenVal)

appraise :: M.Map Int B.ByteString -> Pl -> B.ByteString -> Ev -> IO (Bool,String)
appraise aNonceMap me priKeyBits e =
  case e of
  Mt -> return (True,"")
  G _ e' sig -> do
    let evBits = encodeEv e'
    --priKeyBits <- lookupSecretKeyBytes
    let priKey = SecretKey priKeyBits
    --COM.logc $ "priKey: " ++ (show priKey)
    
    let pubKey = toPublicKey priKey  --TODO: generalize public key management
        sigResult = verify pubKey sig

    (restResult,s) <- appraise aNonceMap me priKeyBits e'
    return ((sigResult && restResult),s)
  N p n bs e' -> do
    if (p/=me)  -- TODO: generalize this (put appraise in COP monad)
      then do
        (restResult,s) <- appraise aNonceMap me priKeyBits e'
        return (restResult, s)
      else
      do    
       let nonceExpectedM = M.lookup n aNonceMap --M.! n
       bitsMatch <-
         case nonceExpectedM of
          Nothing -> do
            putStrLn "Warning:  nonce could not be appraised."
            return True
            {- TODO:  account for the difference between:
               LN NONCE SIG, LN (AT 1 (AT 0 NONCE)) SIG -}
          Just nonceExpected ->
            return (bs == nonceExpected)
       (restResult,s) <- appraise aNonceMap me priKeyBits e'
       return ((bitsMatch && restResult),s)
      
    --TODO:  will need to fix above when we have more than one nonce floating
    --       around..
  U asp_id args p bs e' -> do
    uRes <- appraiseUsm asp_id p args bs
    {-uBits <- getGoldenUsm asp_id p args
    let uRes = (uBits == bs) -}
    (restResult,_) <- appraise aNonceMap me priKeyBits e'
    let uBool = fst uRes
    let uBits = snd uRes
    return ((uBool && restResult), "\nGolden(expected) hash: \n" ++ (show (uBits)) ++ "\n\n" ++ "Actual hash: \n" ++ (show bs))

  K asp_id args p q bs e' -> do
    {-kBits <- getGoldenKim asp_id p args
    let uRes = (kBits == bs)-}
    kRes <- appraiseKim asp_id p args bs
    let kBool = fst kRes
    let kBits = snd kRes
    (restResult,_) <- appraise aNonceMap me priKeyBits e'
    return ((kBool && restResult), "\nGolden(expected) hash: \n" ++ (show kBits) ++ "\n\n" ++ "Actual hash: \n" ++ (show bs))
    


  _ -> return (False,"appraise not implemented")