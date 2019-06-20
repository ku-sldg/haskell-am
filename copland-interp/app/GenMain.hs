{-  Executable that provides an interface for generating arbitrary datatypes, their corresponding JSON representations, and translating between the two.  Primarily for testing and interoperability between different language environments.

  Author: Adam Petz
  Date:  05/29/2019
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GenProgArgs
import Copland
import GenCopland

import Text.Read (readMaybe)
import Data.Maybe(fromJust)
import Data.List(elemIndex)

main :: IO ()
main = do
  opts <- getGenOptions
  --genOptsConsistent opts
  main'' opts
  
  {-reqsToFile 1
  respsToFile 1 -}
  return ()

parseTerm :: String -> T
parseTerm s =
  let (maybeT::Maybe T) = readMaybe s in 
  case maybeT of
   Just t -> t
   _ -> error $ "Failed to parse term" 
  
toTerms :: [String] -> [T]
toTerms ls =
  map parseTerm ls

inLoop :: IO [T]
inLoop = do
      s <- getContents
      let ls = lines s
      let ts = toTerms ls
      return ts

      

  
{-
doTerms :: Int -> FilePath -> FilePath -> IO ()
doTerms n fpIn fpOut = do
  ts <-
    case fpIn of
    "" -> inLoop
    _ -> jsonFromFileToVals fpIn


  termsToFile' fpOut ts
-}

firstTrue :: [Bool] -> Int
firstTrue bs =
  let maybeRes = elemIndex True bs in
  case maybeRes of
   Nothing -> error "should have caught this in command line consistency check"
   Just res -> res


main'' :: Gen_Options -> IO ()
main'' opts@(Gen_Options numTerms reqB respB termB evB inFile outFile dataB) = do

  {-
  putStrLn "hey"
  let s = "Mt"
  let (maybeRes :: Maybe Ev) = readMaybe s
  case maybeRes of
   Just e -> putStrLn $ show e
   Nothing -> error "ah" -}



  
  genOptsConsistent opts

  let ft = firstTrue [reqB, respB, termB, evB]


  case numTerms of
   0 -> do
     case ft of
      0 -> do
        (terms::[RequestMessage]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      1 -> do
        (terms::[ResponseMessage]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      2 -> do
        (terms::[T]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      3 -> do
        (terms::[Ev]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
           
   n -> case ft of
         0 -> reqsToFile outFile dataB n
         1 -> respsToFile outFile dataB n
         2 -> termsToFile outFile dataB n
         3 -> evsToFile outFile dataB n

  
{-
main'' :: Gen_Options -> IO ()
main'' (Gen_Options numTerms reqB respB termB evB inFile outFile dataB) = do
  return ()

  case numTerms of
   0 -> case numResps of
         0 ->
           case numTerms of
            0 ->
              case numEvs of
               0 ->
                 case reqIn of
                  "" -> 
                    case respIn of
                     "" ->
                       case termIn of
                        "" ->
                          case evIn of
                           "" -> return ()
                           fp -> return ()
                        fp -> return ()
                     fp -> return ()
                  fp -> return ()
               n -> evsJsonToFile evOut n
            n -> termsJsonToFile termOut n
         n -> respsJsonToFile respOut n
   n -> reqsJsonToFile reqOut n
     -}





  {-
  --doTerms numTerms termIn termOut
  --termsJsonToFile "termsTest.txt" 2
  (ts :: [T]) <- jsonFromFileToVals "termsTest.txt"
  --error "helllo"
  putStrLn $ show ts -}
  

      

  
