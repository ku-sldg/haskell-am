{-  Generators of random Copland-related datatypes.  Uses Haskell's QuickCheck infrastructure.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}
module QcCopland where


import Copland

import Test.QuickCheck
import qualified Data.ByteString as B (empty)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Binary as BI (encode)
import qualified Data.Map as M
import Control.Monad(replicateM)

{----- Shared qc -----}
maxPl :: Pl
maxPl = 10

maxArgs :: Int
maxArgs = 3

argString :: Int -> ARG
argString n =
  let numStr = show n in
  "arg" ++ numStr

argList :: Int -> [ARG]
argList n =
  let nums = [1..n] in
  map argString nums

genArgs :: Gen [ARG]
genArgs = do
  numArgs <- choose (0,maxArgs)
  return (argList numArgs)

genPl :: Gen Pl
genPl = choose (0, maxPl)

maxNumNames :: Int
maxNumNames = 5

genAddress :: Gen Address
genAddress = do
  let (addrRange :: (Int,Int)) = (3000,4000) 
  res <- choose addrRange
  return (show res)

genPlAddr :: Gen (Pl,Address)
genPlAddr = do
  pl <- genPl
  addr <- genAddress
  return (pl,addr)

genNameMap :: Gen (M.Map Pl Address)
genNameMap = do
  numNames <- choose (0,maxNumNames)
  l <- replicateM numNames genPlAddr
  return (M.fromList l)
  

{-
instance Arbitrary Address where
  arbitrary = sized $ \n -> genAddress
-}

{---------- Messages qc ----------}

genReqMessage :: Int -> Gen RequestMessage
genReqMessage n = do
  p <- genPl
  q <- genPl
  m <- genNameMap
  t <- genTerm n
  e <- genEv n
  return $ RequestMessage p q m t e

genRespMessage :: Int -> Gen ResponseMessage
genRespMessage n = do
  p <- genPl
  q <- genPl
  e <- genEv n
  return $ ResponseMessage p q e

{---------- Term qc ----------}

instance Arbitrary T where
  arbitrary = sized $ \n -> genTerm (rem n 25)
  
genTerm :: Int -> Gen T
genTerm n =
  case n of 0 ->
              do
                term <- oneof [genKIM, genUSM, {-genNONCE,-} genHSH, genSIG]
                return term
            _ -> do
              term <-
                oneof [genLN (n-1), genBRs (n-1), genBRp (n-1),
                       genAT (n-1), genKIM, genUSM, {-genNONCE,-} genHSH, genSIG]
              return term

{-
genNONCE :: Gen T
genNONCE = do
  return (NONCE) -}

genSIG :: Gen T
genSIG = do
  return (SIG)

genHSH :: Gen T
genHSH = do
  return (HSH)

genKIM = do
  i <- genPl
  args <- genArgs
  p <- genPl
  return (KIM i p args)

genUSM = do
  i <- genPl
  args <- genArgs
  return (USM i args)

genLN n = do
  t0 <- genTerm n
  t1 <- genTerm n
  return (LN t0 t1)

genAT n = do
  p <- genPl
  t <- genTerm n
  return (AT p t)

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
  return (BRS (sp1,sp2) t0 t1)

genBRp n = do
  sp1 <- genSP n
  sp2 <- genSP n
  t0 <- genTerm n
  t1 <- genTerm n
  return (BRP (sp1,sp2) t0 t1)


{---------- Evidence qc ----------}

instance Arbitrary Ev where
  arbitrary = sized $ \n -> genEv (rem n 25)
  
genEv :: Int -> Gen Ev
genEv n =
  case n of 0 ->
              do
                term <- oneof [genMt]
                return term
            _ -> do
              let r = 1
                  p = 2
              term <-
                frequency [(r,(genSS (n-1))),
                           (r,(genPP (n-1))),
                           (r,(genN (n-1))),
                           (r,(genG (n-1))),
                           (r,(genK (n-1))),
                           (r,(genU (n-1))),
                           (p,genH),
                           (p,genMt)]
              return term

genBS :: Gen BS
genBS = do
  e <- genPl --(genTerm 5)
  let bits = BI.encode e
  return (BL.toStrict bits)

genMt :: Gen Ev
genMt = return Mt

genU n = do
  i <- genPl
  args <- genArgs
  p <- genPl
  bs <- genBS
  ev <- genEv n
  return $ U i args p bs ev

genK n = do
  i <- genPl
  args <- genArgs
  q <- genPl
  p <- genPl
  bs <- genBS
  ev <- genEv n
  return $ K i args q p bs ev

genG n = do
  p <- genPl
  bs <- genBS
  ev <- genEv n
  return $ G p ev bs

genH :: Gen Ev
genH = do
  p <- genPl
  bs <- genBS
  return $ H p bs

genN n = do
  p <- genPl
  n <- genPl
  bs <- genBS
  ev <- genEv n
  return $ N p n bs ev

genSS n = do
  ev1 <- genEv n
  ev2 <- genEv n
  return $ SS ev1 ev2

genPP n = do
  ev1 <- genEv n
  ev2 <- genEv n
  return $ PP ev1 ev2
