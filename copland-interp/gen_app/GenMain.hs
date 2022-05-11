{-  Executable that provides an interface for generating arbitrary datatypes, their corresponding JSON representations, and translating between the two.  Primarily for testing and interoperability between different language environments.

  Author: Adam Petz
  Date:  05/29/2019
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GenProgArgs
import Copland
import Copland_Gen
import ConcreteEvidence

import qualified Copland_Concrete as CT
import qualified Example_Phrases_Concrete as EPC

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

local_vals_term :: [Term]
local_vals_term = [CT.toExtractedTerm EPC.simple_bpar_phrase] --EPC.simple_asp_phrase]
  --[CT.toExtractedTerm EPC.cert_style]

local_vals_req :: [RequestMessage]
local_vals_req = undefined

local_vals_resp :: [ResponseMessage]
local_vals_resp = undefined

local_vals_evc :: [EvidenceC]
local_vals_evc = undefined

main'' :: Gen_Options -> IO ()
main'' opts@(Gen_Options numTerms reqB respB termB evB inFile outFile dataB localB) = do

  genOptsConsistent opts

  let ft = firstTrue [reqB, respB, termB, evB]

  case numTerms of
   0 -> do
     case ft of
      0 -> do
        (terms::[RequestMessage]) <- case localB of
          True -> return local_vals_req
          False -> case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      1 -> do
        (terms::[ResponseMessage]) <- case localB of
          True -> return local_vals_resp
          False -> case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      2 -> do
        (terms::[Term]) <- case localB of
          True -> return local_vals_term
          False -> case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
      3 -> do
        (terms::[EvidenceC]) <- case localB of
          True -> return local_vals_evc
          False -> case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile
        termsToFile' outFile dataB terms
           
   n -> case ft of
         0 -> reqsToFile outFile dataB n
         1 -> respsToFile outFile dataB n
         2 -> termsToFile outFile dataB n
         3 -> evsToFile outFile dataB n
