module Test_Extract where

import StVM (Coq_cvm_st(..))
import Term_Defs (ASP(..),ASP_PARAMS(..),EvC(..), Evidence(..))
import Anno_Term_Defs (AnnoTermPar(..))
import Term_Defs_Deriving
import StVM_Deriving
import Impl_VM_Extracted (run_cvm)
import CryptoImpl (doNonce)
import MonadCop (Cop_Env(..))
import DemoStates

main :: IO ()
main = do
  let t' = Coq_aasp_par (ASPC (Coq_asp_paramsC 1 [] 1 1)) --(Coq_aasp_par (CPY))
  let tsig = (Coq_aasp_par SIG)
  let t = Coq_alseq_par t' tsig
  nval <- doNonce
  let st = (Coq_mk_st (Coq_evc [nval] (Coq_nn 1)) [] 0 0)
  let env = undefined --Cop_Env undefined undefined undefined undefined undefined undefined undefined undefined
  putStrLn $ "\n" ++ "Term executed: \n" ++ (show t) ++ "\n"
  putStrLn $ "Starting CVM state: \n" ++ (show st) ++ "\n"
  --putStrLn $ "Starting CVM env: \n" ++ (show env) ++ "\n"
  res <- run_cvm t st env -- TODO: get real Cop_Env
  putStrLn $ "Result CVM state: \n" ++ (show res) ++ "\n"
 
















--import IO_Stubs
--import CoplandLang_Extracted(AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))
--import MonadCVM (run_cvm, Cvm_st(..), AnnoTermPar(..), ASP(..),EvC(..), Evidence(..))


{-
copland_compile :: CVM ()
copland_compile = undefined
-}
