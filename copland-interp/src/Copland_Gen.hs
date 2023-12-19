{-  Higher-level interface for random generation, input, and output of datatypes and their JSON representations.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes, ScopedTypeVariables #-}

module Copland_Gen where

import Copland
import Copland_Qc
import ConcreteEvidence

import Test.QuickCheck (Gen, generate, vectorOf, Arbitrary, arbitrary, resize)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Maybe (fromJust)
import Text.Read(readMaybe)
import Control.Exception
import System.IO.Error hiding (catch)
import qualified Data.ByteString.Lazy as BL (appendFile, append, readFile, ByteString, writeFile, getContents)
import qualified System.Directory as SD (removeFile)
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn,lines)

appendJsonToFile :: (ToJSON a,FromJSON a,Read a,Show a{-,Arbitrary a-}) => FilePath -> Bool -> a -> IO ()
appendJsonToFile fp b t = do
  case b of
   True -> do
     let s = show t
     case fp of
      "" -> putStrLn s
      _ -> let sl = s ++ "\n" in
            appendFile fp sl
     
   False -> do
     let bs = encode t
     case fp of
      "" -> C.putStrLn bs
      _ -> let bsl = BL.append bs "\n" in
            BL.appendFile fp bsl

termsToFile' :: (ToJSON a,FromJSON a,Read a,Show a{-,Arbitrary a-}) => FilePath -> Bool -> [a] -> IO ()
termsToFile' fp b ts = do
  SD.removeFile fp `catch` handleExists 
  mapM_ (appendJsonToFile fp b) ts

 where handleExists e
         | isDoesNotExistError e = return ()
         | otherwise = throwIO e

jsonToVals :: (ToJSON a,FromJSON a,Read a,Show a,Arbitrary a) => BL.ByteString -> [a]
jsonToVals bs =
  let bsl = C.lines bs in
  let res = map (fromJust . decode) bsl in
  res

stringToVals :: (ToJSON a,FromJSON a,Read a,Show a,Arbitrary a) => String -> [a]
stringToVals str = do
  let bsl = lines str
      res = map (fromJust . readMaybe) bsl in res
  --return res

fromFileToVals :: (ToJSON a,FromJSON a,Read a,Show a,Arbitrary a) => FilePath -> IO [a]
fromFileToVals fp = do
  str <-
    case fp of
     "" -> getContents
     _  -> do putStrLn $ "reading from file: " ++ fp
              readFile fp

  let res = stringToVals str
  return res

  
jsonFromFileToVals :: (ToJSON a,FromJSON a,Read a,Show a,Arbitrary a) => FilePath -> IO [a]
jsonFromFileToVals fp = do
  bs <- 
    case fp of
     "" -> BL.getContents
     _ -> BL.readFile fp       
  let res = jsonToVals bs
  return res

defaultSize :: Int
defaultSize = 30

{----- RequestMessage samples to file -----}

reqsToFile :: FilePath -> Bool -> Int -> IO ()
reqsToFile fp b n = do
  (terms::[RequestMessage]) <- sampleNarb n --sampleN' n (genTerm defaultSize)
  termsToFile' fp b terms

{----- ResponseMessage samples to file -----}

respsToFile :: FilePath -> Bool -> Int -> IO ()
respsToFile fp b n = do
  (terms::[ResponseMessage]) <- sampleNarb n
  termsToFile' fp b terms

{----- Term samples to file -----}

termsToFile :: FilePath -> Bool -> Int -> IO ()
termsToFile fp b n = do
  (terms::[Term]) <- sampleNarb n
  termsToFile' fp b terms

{----- Ev samples to file -----}

evsToFile :: FilePath -> Bool -> Int -> IO ()
evsToFile fp b n = do
  --(evs :: [RawEv]) <- return [] --(evs::[RawEv]) <- sampleNarb n
  evs :: [EvidenceC] <- sampleNarb n
  termsToFile' fp b evs

arbsToFile :: forall a . (ToJSON a, FromJSON a,Read a,Show a,Arbitrary a) => FilePath -> Bool -> Int -> IO ()
arbsToFile fp b n = do
  (evs::[a]) <- sampleNarb n
  termsToFile' fp b evs

sampleNarb :: (ToJSON a, FromJSON a,Read a,Show a,Arbitrary a) => Int -> IO [a]
sampleNarb n = sampleN' n (resize defaultSize arbitrary)

-- | Generates <first param> example values from <second param> generator
sampleN' :: (ToJSON a,FromJSON a,Read a,Show a,Arbitrary a) => Int -> Gen a -> IO [a]
sampleN' n g =
  generate (vectorOf n g)

{-
-- | Generates <first param> example values from <second param> generator
--   and prints them to 'stdout'.
sampleN :: Show a => Int -> Gen a -> IO ()
sampleN n g =
  do cases <- sampleN' n g
     mapM_ print cases -}
