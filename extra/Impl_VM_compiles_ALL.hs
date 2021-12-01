module Impl_VM where

--import CoplandLang_Extracted(AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))
import MonadCVM (run_cvm, Cvm_st(..), AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))
import CryptoImpl (doNonce)
--import IO_Stubs

{-
copland_compile :: CVM ()
copland_compile = undefined
-}


main :: IO ()
main = do
  let t' = (Aasp_par (CPY))
  let tsig = (Aasp_par SIG)
  let t = Alseq_par t' tsig
  nval <- doNonce
  let st = (Mk_st (Evc [nval] (Nn 1)) [] 0 0)
  putStrLn $ show t
  putStrLn $ show st
  res <- run_cvm t st
  putStrLn $ show res
 
