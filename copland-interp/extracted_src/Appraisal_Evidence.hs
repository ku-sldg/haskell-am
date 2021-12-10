module Appraisal_Evidence where

import qualified Prelude
import qualified BS
import qualified GenOptMonad
import qualified Term_Defs

peel_bs :: Term_Defs.RawEv -> GenOptMonad.Opt ((,) BS.BS Term_Defs.RawEv)
peel_bs ls =
  case ls of {
   ([]) -> GenOptMonad.failm;
   (:) bs ls' -> GenOptMonad.ret ((,) bs ls')}

