module Impl_VM_Extracted where

import qualified Term_Defs
import qualified StVM
import qualified GenStMonad
import Control.Monad.IO.Class (liftIO)
import MonadCop (COP, Cop_Env, runCOP)
import Impl_VM (copland_compile) --hiding (run_cvm)
import Control.Monad.Reader(runReaderT)

run_cvm :: Term_Defs.AnnoTermPar -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm t st env =
  runCOP (GenStMonad.execSt (copland_compile t) st) env
