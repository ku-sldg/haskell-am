module Impl_VM_Extracted where

import qualified Term_Defs (Term, AnnoTermPar, annotated_par, RawEv, get_bits)
import qualified StVM
import qualified GenStMonad
import Control.Monad.IO.Class (liftIO)
import MonadCop (COP, Cop_Env, runCOP)
import Impl_VM (copland_compile) --hiding (run_cvm)
import Control.Monad.Reader(runReaderT)

run_cvm :: Term_Defs.AnnoTermPar -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm t st env =
  runCOP (GenStMonad.execSt (copland_compile t) st) env

run_cvm' :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm' t st env = run_cvm (Term_Defs.annotated_par t) st env

run_cvm_rawev :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO Term_Defs.RawEv
run_cvm_rawev t st env = do
  res_st <- run_cvm' t st env
  return (Term_Defs.get_bits (StVM.st_ev res_st))
