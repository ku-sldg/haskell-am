{-  Executable that provides an interface for generating arbitrary datatypes, their corresponding JSON representations, and translating between the two.  Primarily for testing and interoperability between different language environments.

  Author: Adam Petz
  Date:  05/29/2019
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GenProgArgs
import Copland
import GenCopland
import ConcreteEvidence

import Data.List(elemIndex)

main :: IO ()
main = do
  opts <- getGenOptions
  main'' opts
  return ()

firstTrue :: [Bool] -> Int
firstTrue bs =
  let maybeRes = elemIndex True bs in
  case maybeRes of
   Nothing -> error "should have caught this in command line consistency check"
   Just res -> res


main'' :: Gen_Options -> IO ()
main'' opts@(Gen_Options numTerms reqB respB termB evB inFile outFile dataB) = do

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
        (terms::[Term]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      3 -> do
        (terms::[EvidenceC]) <- case dataB of
                  True -> jsonFromFileToVals inFile
                  False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
           
   n -> case ft of
         0 -> reqsToFile outFile dataB n
         1 -> respsToFile outFile dataB n
         2 -> termsToFile outFile dataB n
         3 -> evsToFile outFile dataB n
