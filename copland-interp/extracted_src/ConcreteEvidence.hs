module ConcreteEvidence where

import qualified Prelude
import qualified BS
import qualified Term_Defs

data EvidenceC =
   Coq_mtc
 | Coq_nnc Term_Defs.N_ID BS.BS
 | Coq_uuc Term_Defs.ASP_PARAMS Term_Defs.Plc BS.BS EvidenceC
 | Coq_ggc Term_Defs.Plc BS.BS EvidenceC
 | Coq_hhc Term_Defs.Plc BS.BS Term_Defs.Evidence
 | Coq_ssc EvidenceC EvidenceC
 | Coq_ppc EvidenceC EvidenceC

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

