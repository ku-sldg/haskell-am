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
import qualified BS as BS

import Data.List(elemIndex)
import qualified Data.Map as M

main :: IO ()
main = do
  opts <- getGenOptions
  main' opts
  return ()

local_vals_t :: Term
local_vals_t = CT.toExtractedTerm (EPC.fileHash_asp "target.txt")
  --EPC.simple_asp_phrase
  --EPC.simple_asp_phrase
  --EPC.cert_style
  
local_vals_term :: [Term]
local_vals_term = [local_vals_t]

local_vals_req :: [RequestMessage]
local_vals_req = [ (RequestMessage
                    from_plc
                    to_plc
                    cvm_address_map
                    t
                    init_evidence) ]
                 where to_plc = 1
                       from_plc = 0
                       t = local_vals_t
                       cvm_address_map = (M.fromList [(1,"CVM_1")])
                       init_evidence = [BS.zero_bs]
{-
  [RequestMessage 1 2 (M.fromList [(1,"addr1"), (2,"addr2")])
    local_vals_t [BS.zero_bs, BS.one_bs]]
-}

local_vals_resp :: [ResponseMessage]
local_vals_resp = [ResponseMessage 1 2 [BS.zero_bs, BS.one_bs]]

local_vals_sig_req :: [SigRequestMessage]
local_vals_sig_req = [SigRequestMessage BS.zero_bs]

local_vals_sig_resp :: [SigResponseMessage]
local_vals_sig_resp = [SigResponseMessage BS.one_bs]

local_vals_asp_req :: [AspRequestMessage]
local_vals_asp_req = [AspRequestMessage
                        (Coq_asp_paramsC 1 ["arg1"] 2 3)
                        [BS.zero_bs, BS.one_bs]]

local_vals_asp_resp :: [AspResponseMessage]
local_vals_asp_resp = [AspResponseMessage BS.one_bs]

local_vals_evt :: [Evidence]
local_vals_evt = [Coq_pp (Coq_nn 1) (Coq_nn 2)]
  --[Coq_hh 1 (Coq_nn 5)]
  --[Coq_uu (Coq_asp_paramsC 1 ["arg1"] 2 3) 4 (Coq_nn 5)]

local_vals_evc :: [EvidenceC] -- BS.one_bs
local_vals_evc = [Coq_ssc (Coq_nnc 1 BS.zero_bs) (Coq_nnc 0 BS.one_bs)]
  --[Coq_hhc 1 BS.one_bs (Coq_nn 1)]
  --[Coq_ggc 1 BS.one_bs (Coq_nnc 1 BS.zero_bs)]
  --[Coq_uuc (Coq_asp_paramsC 1 ["arg1"] 2 3) 4 BS.one_bs (Coq_nnc 1 BS.one_bs)]
  --[Coq_nnc 1 BS.one_bs]

main' :: Gen_Options -> IO ()
main' opts@(Gen_Options numTerms reqB respB termB evB evtB sigReqB sigRespB aspReqB aspRespB inFile outFile dataB localB) = do

  genOptsConsistent opts

  let ft = firstTrue [reqB, respB, termB, evB, evtB, sigReqB, sigRespB, aspReqB, aspRespB]

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
      4 -> do
        (terms::[Evidence]) <- case localB of
          True -> return local_vals_evt
          False -> return []  -- TODO:  implement random generator for Evidence type
            {- case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile -}
        termsToFile' outFile dataB terms
      5 -> do
        (terms::[SigRequestMessage]) <- case localB of
          True -> return local_vals_sig_req
          False -> return [] -- TODO:  implement random generator for this type
          {-
            case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile -}
        termsToFile' outFile dataB terms 
      6 -> do
        (terms::[SigResponseMessage]) <- case localB of
          True -> return local_vals_sig_resp
          False -> return [] -- TODO:  implement random generator for this type
          {-
            case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile -}
        termsToFile' outFile dataB terms

      7 -> do
        (terms::[AspRequestMessage]) <- case localB of
          True -> return local_vals_asp_req
          False -> return [] -- TODO:  implement random generator for this type
          {-
            case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile -}
        termsToFile' outFile dataB terms 
      8 -> do
        (terms::[AspResponseMessage]) <- case localB of
          True -> return local_vals_asp_resp
          False -> return [] -- TODO:  implement random generator for this type
          {-
            case dataB of
                     True -> jsonFromFileToVals inFile
                     False -> fromFileToVals inFile -}
        termsToFile' outFile dataB terms
           
   n -> case ft of
         0 -> reqsToFile outFile dataB n
         1 -> respsToFile outFile dataB n
         2 -> termsToFile outFile dataB n
         3 -> evsToFile outFile dataB n




firstTrue :: [Bool] -> Int
firstTrue bs =
  let maybeRes = elemIndex True bs in
  case maybeRes of
   Nothing -> error "should have caught this in command line consistency check"
   Just res -> res
