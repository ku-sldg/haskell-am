module Test_Extract where

--import CoplandLang_Extracted(AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))
--import MonadCVM (run_cvm, Cvm_st(..), AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))

import StVM (Coq_cvm_st(..))
import Term_Defs (AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))
import Term_Defs_Deriving
import Impl_VM_Extracted (run_cvm)
import CryptoImpl (doNonce)
--import IO_Stubs

{-
copland_compile :: CVM ()
copland_compile = undefined
-}


main :: IO ()
main = do
  let t' = (Coq_aasp_par (CPY))
  let tsig = (Coq_aasp_par SIG)
  let t = Coq_alseq_par t' tsig
  nval <- doNonce
  let st = (Coq_mk_st (Coq_evc [nval] (Coq_nn 1)) [] 0 0)
  putStrLn $ show t
  putStrLn $ show st
  res <- run_cvm t st
  putStrLn $ show res
 
