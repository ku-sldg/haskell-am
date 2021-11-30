{-  Generators of random Copland-related datatypes.  Uses Haskell's QuickCheck infrastructure.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module QcCopland where


import Copland
import BS

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, sized, oneof, frequency)
import Control.Monad(replicateM)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Binary as BI (encode)
import qualified Data.Map as M

{----- Shared qc -----}
maxPl :: Plc
maxPl = 10

maxArgs :: Int
maxArgs = 3

maxEvLength :: Int
maxEvLength = 5

{-
genBS :: Gen BS
genBS = do
  p <- genPl
  let bits = BI.encode p
  return (BL.toStrict bits)
-}

argString :: Int -> Arg
argString n =
  let numStr = show n in
  "arg" ++ numStr
  {-let s = "arg" ++ numStr in
  BL.toStrict $ BI.encode s -}

argList :: Int -> [Arg]
argList n =
  let nums = [1..n] in
  map argString nums

genArgs :: Gen [Arg]
genArgs = do
  numArgs <- choose (0,maxArgs)
  return (argList numArgs)

genPl :: Gen Plc
genPl = choose (0, maxPl)

maxNumNames :: Int
maxNumNames = 5

genAddress :: Gen Address
genAddress = do
  let (addrRange :: (Int,Int)) = (3000,4000) 
  res <- choose addrRange
  return (show res)

genPlAddr :: Gen (Plc,Address)
genPlAddr = do
  pl <- genPl
  addr <- genAddress
  return (pl,addr)

genNameMap :: Gen (M.Map Plc Address)
genNameMap = do
  numNames <- choose (0,maxNumNames)
  l <- replicateM numNames genPlAddr
  return (M.fromList l)

{---------- Messages qc ----------}

genReqMessage :: Int -> Gen RequestMessage
genReqMessage n = do
  p <- genPl
  q <- genPl
  m <- genNameMap
  t <- genTerm n
  e <- genEv --n
  return $ RequestMessage p q m t e

instance Arbitrary ResponseMessage where
  arbitrary = sized $ \n -> genRespMessage (rem n 25)
  
genRespMessage :: Int -> Gen ResponseMessage
genRespMessage n = do
  p <- genPl
  q <- genPl
  e <- genEv --n
  return $ ResponseMessage p q e

instance Arbitrary RequestMessage where
  arbitrary = sized $ \n -> genReqMessage (rem n 25)
  
{---------- Term qc ----------}

instance Arbitrary Term where
  arbitrary = sized $ \n -> genTerm (rem n 25)
  
genTerm :: Int -> Gen Term
genTerm n =
  let base_gen = oneof [genUSM, genHSH, genSIG] in
  case n of 0 -> base_gen
            _ -> frequency $ [(n,genAT (n-1)),
                              (n,genLN (n-1)),
                              (n,genBRs (n-1)),
                              (n,genBRp (n-1)),
                              (1,base_gen)]

genSIG :: Gen Term
genSIG = do
  return (Coq_asp SIG)

genHSH :: Gen Term
genHSH = do
  return (Coq_asp HSH)

genUSM = do
  i <- genPl
  args <- genArgs
  tpl <- genPl
  tid <- genPl
  return (Coq_asp $ ASPC $ Coq_asp_paramsC i args tpl tid)

genLN n = do
  t0 <- genTerm n
  t1 <- genTerm n
  return (Coq_lseq t0 t1)

genAT n = do
  p <- genPl
  t <- genTerm n
  return (Coq_att p t)

genSP n = do
  x <- oneof [return True, return False]
  case x of
   False -> return NONE
   True -> return ALL

genBRs n = do
  sp1 <- genSP n
  sp2 <- genSP n
  t0 <- genTerm n
  t1 <- genTerm n
  return (Coq_bseq (sp1,sp2) t0 t1)

genBRp n = do
  sp1 <- genSP n
  sp2 <- genSP n
  t0 <- genTerm n
  t1 <- genTerm n
  return (Coq_bpar (sp1,sp2) t0 t1)


{---------- Evidence qc ----------}





genBS :: Gen BS
genBS = do
  p <- genPl
  let bits = BI.encode p
  return (BL.toStrict bits)
  

evString :: Int -> BS
evString n =
  {-let numStr = show n in
  "arg" ++ numStr -}
  {-let s = "arg" ++ numStr in
  BL.toStrict $ BI.encode s -}
  BL.toStrict $ BI.encode n


evList :: Int -> [BS]
evList n =
  let nums = [1..n] in
  map evString nums

genEv :: Gen [BS]
genEv = do
  numArgs <- choose (0,maxEvLength)
  return (evList numArgs)

{-
instance Arbitrary RawEv where
  arbitrary = genEv --sized $ \n -> genEv --sized $ \n -> genEv (rem n 25)
-}
{-
genEv :: Gen RawEv
genEv = return [] -- TODO: make this more sophisicated?
-}


{-
instance Arbitrary EvidenceC where
  arbitrary = sized $ \n -> genEv (rem n 25)
  
genEv :: Int -> Gen EvidenceC
genEv n =
  case n of 0 -> oneof [genMt]
            _ ->
              let r = 1
                  p = 2 in
              frequency [(r,(genSS (n-1))),
                         (r,(genPP (n-1))),
                         (r,(genN (n-1))),
                         (r,(genG (n-1))),
                         (r,(genU (n-1))),
                         (p,genH),
                         (p,genMt)]
-}

{-
genBS :: Gen BS
genBS = do
  p <- genPl
  let bits = BI.encode p
  return (BL.toStrict bits)
-}


{-
genMt :: Gen EvidenceC
genMt = return Mt

genU n = do
  i <- genPl
  p <- genPl
  tpl <- genPl
  tid <- genPl
  args <- genArgs
  bs <- genBS
  ev <- genEv n
  return $ U (ASP_PARAMSC i args tpl tid) p bs ev

genG n = do
  p <- genPl
  bs <- genBS
  ev <- genEv n
  return $ G p bs ev

genEvT :: Gen Evidence
genEvT = return Mtt -- TODO:  make this real

genH :: Gen EvidenceC
genH = do
  bs <- genBS
  p <- genPl
  et <- genEvT
  return $ H p bs et

genN n = do
  p <- genPl
  n <- genPl
  bs <- genBS
  ev <- genEv n
  return $ N n bs ev

genSS n = do
  ev1 <- genEv n
  ev2 <- genEv n
  return $ SS ev1 ev2

genPP n = do
  ev1 <- genEv n
  ev2 <- genEv n
  return $ PP ev1 ev2
-}
