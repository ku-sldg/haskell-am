module Appraisal_Evidence where

import qualified Prelude
import qualified BS
import qualified OptMonad_Coq
import qualified Term_Defs

peel_bs :: Term_Defs.RawEv -> OptMonad_Coq.Opt ((,) BS.BS Term_Defs.RawEv)
peel_bs ls =
  case ls of {
   ([]) -> OptMonad_Coq.failm;
   (:) bs ls' -> OptMonad_Coq.ret ((,) bs ls')}

