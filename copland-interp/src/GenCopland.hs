{-  Higher-level interface for random generation, input, and output of datatypes and their JSON representations.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE OverloadedStrings #-}

module GenCopland where

import Copland
import QcCopland

import Test.QuickCheck (Gen, generate)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Maybe (fromJust)
import Text.Read(readMaybe)
import Control.Exception
import System.IO.Error hiding (catch)
import qualified Data.ByteString.Lazy as BL (appendFile, append, readFile, ByteString, writeFile, putStrLn, getContents)
import qualified System.Directory as SD (removeFile)
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn,lines)

appendJsonToFile :: (ToJSON a, FromJSON a,Read a,Show a) => FilePath -> Bool -> a -> IO ()
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

termsToFile' :: (ToJSON a, FromJSON a,Read a,Show a) => FilePath -> Bool -> [a] -> IO ()
termsToFile' fp b ts = do
  SD.removeFile fp `catch` handleExists 
  mapM_ (appendJsonToFile fp b) ts

 where handleExists e
         | isDoesNotExistError e = return ()
         | otherwise = throwIO e

jsonToVals :: (ToJSON a, FromJSON a,Read a,Show a) => BL.ByteString -> [a]
jsonToVals bs =
  let bsl = C.lines bs in
  let res = map (fromJust . decode) bsl in
  res

stringToVals :: (ToJSON a, FromJSON a,Read a,Show a) => String -> IO [a]
stringToVals str = do
  --let asdf = error str in
  --putStrLn "hi"
  let bsl = lines str
 {- putStrLn $ show (length bsl)
  --error (show bsl)
  let s = bsl !! 0
  putStrLn $ s
  let (ms::Maybe Ev) = readMaybe s
  putStrLn (show ms) -}
  --putStrLn $ show bsl
  let res = map (fromJust . readMaybe) bsl
  return res

fromFileToVals :: (ToJSON a, FromJSON a,Read a,Show a) => FilePath -> IO [a]
fromFileToVals fp = do
  str <-
    case fp of
     "" -> getContents
     _  -> do putStrLn $ "reading from file: " ++ fp
              readFile fp

  --error str
  res <- stringToVals str
  return res

  
jsonFromFileToVals :: (ToJSON a, FromJSON a,Read a,Show a) => FilePath -> IO [a]
jsonFromFileToVals fp = do
  bs <- 
    case fp of
     "" -> BL.getContents
     _ -> BL.readFile fp       
  let res = jsonToVals bs
  return res

defaultSize :: Int
defaultSize = 30

{----- RequestMessage json samples to file -----}
  
reqsToFile :: FilePath -> Bool -> Int -> IO ()
reqsToFile fp b n = do
  terms <- sampleN' n (genReqMessage defaultSize)
  termsToFile' fp b terms

{----- ResponseMessage samples to file -----}
  
respsToFile :: FilePath -> Bool -> Int -> IO ()
respsToFile fp b n = do
  terms <- sampleN' n (genRespMessage defaultSize)
  termsToFile' fp b terms

{----- Term samples to file -----}

termsToFile :: FilePath -> Bool -> Int -> IO ()
termsToFile fp b n = do
  terms <- sampleN' n (genTerm defaultSize)
  termsToFile' fp b terms

{----- Ev samples to file -----}
  
evsToFile :: FilePath -> Bool -> Int -> IO ()
evsToFile fp b n = do
  evs <- sampleN' n (genEv defaultSize)
  termsToFile' fp b evs

-- | Generates <first param> example values from <second param> generator
sampleN' :: Int -> Gen a -> IO [a]
sampleN' n g =
  generate (sequence (replicate n g))


{-
-- | Generates <first param> example values from <second param> generator
--   and prints them to 'stdout'.
sampleN :: Show a => Int -> Gen a -> IO ()
sampleN n g =
  do cases <- sampleN' n g
     mapM_ print cases -}
