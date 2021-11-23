module Impl_VM_Extracted where

import qualified Term_Defs
import qualified StVM
import qualified GenStMonad
import Impl_VM (copland_compile) --hiding (run_cvm)

run_cvm :: Term_Defs.AnnoTermPar -> StVM.Coq_cvm_st -> Prelude.IO StVM.Coq_cvm_st
run_cvm t st =
  GenStMonad.execSt (copland_compile t) st
