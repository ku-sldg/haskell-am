module ConcreteEvidence where

import qualified Prelude
import qualified Term_Defs

splitEv_l :: Term_Defs.Split -> Term_Defs.EvC -> Term_Defs.EvC
splitEv_l sp e =
  case sp of {
   (,) s _ ->
    case s of {
     Term_Defs.ALL -> e;
     Term_Defs.NONE -> Term_Defs.mt_evc}}

splitEv_r :: Term_Defs.Split -> Term_Defs.EvC -> Term_Defs.EvC
splitEv_r sp e =
  case sp of {
   (,) _ s0 ->
    case s0 of {
     Term_Defs.ALL -> e;
     Term_Defs.NONE -> Term_Defs.mt_evc}}

