module Impl_appraisal where

import qualified Prelude
import qualified Appraisal_Defs
import qualified Appraisal_Evidence
import qualified Appraisal_IO_Stubs
import qualified ConcreteEvidence
import qualified List
import qualified OptMonad_Coq
import qualified Term_Defs

build_app_comp_evC :: Term_Defs.Evidence -> Term_Defs.RawEv ->
                      OptMonad_Coq.Opt ConcreteEvidence.EvidenceC
build_app_comp_evC et ls =
  case et of {
   Term_Defs.Coq_mt -> OptMonad_Coq.ret ConcreteEvidence.Coq_mtc;
   Term_Defs.Coq_uu params p et' ->
    OptMonad_Coq.bind (Appraisal_Evidence.peel_bs ls) (\x0 ->
      case x0 of {
       (,) bs ls' ->
        OptMonad_Coq.bind (build_app_comp_evC et' ls') (\x ->
          OptMonad_Coq.bind (Appraisal_IO_Stubs.checkASP params bs) (\res ->
            OptMonad_Coq.ret (ConcreteEvidence.Coq_uuc params p res x)))});
   Term_Defs.Coq_gg p et' ->
    OptMonad_Coq.bind (Appraisal_Evidence.peel_bs ls) (\x0 ->
      case x0 of {
       (,) bs ls' ->
        OptMonad_Coq.bind (build_app_comp_evC et' ls') (\x ->
          OptMonad_Coq.bind (Appraisal_IO_Stubs.checkSigBits ls' p bs)
            (\res -> OptMonad_Coq.ret (ConcreteEvidence.Coq_ggc p res x)))});
   Term_Defs.Coq_hh p et0 ->
    OptMonad_Coq.bind (Appraisal_Evidence.peel_bs ls) (\x ->
      case x of {
       (,) bs _ ->
        OptMonad_Coq.bind (Appraisal_Defs.checkHash et0 p bs) (\res ->
          OptMonad_Coq.ret (ConcreteEvidence.Coq_hhc p res et0))});
   Term_Defs.Coq_nn nid ->
    OptMonad_Coq.bind (Appraisal_Evidence.peel_bs ls) (\x ->
      case x of {
       (,) bs _ ->
        OptMonad_Coq.bind (Appraisal_IO_Stubs.checkNonce nid bs) (\res ->
          OptMonad_Coq.ret (ConcreteEvidence.Coq_nnc nid res))});
   Term_Defs.Coq_ss et1 et2 ->
    OptMonad_Coq.bind
      (build_app_comp_evC et1 (List.firstn (Term_Defs.et_size et1) ls))
      (\x ->
      OptMonad_Coq.bind
        (build_app_comp_evC et2 (List.skipn (Term_Defs.et_size et1) ls))
        (\y -> OptMonad_Coq.ret (ConcreteEvidence.Coq_ssc x y)));
   Term_Defs.Coq_pp et1 et2 ->
    OptMonad_Coq.bind
      (build_app_comp_evC et1 (List.firstn (Term_Defs.et_size et1) ls))
      (\x ->
      OptMonad_Coq.bind
        (build_app_comp_evC et2 (List.skipn (Term_Defs.et_size et1) ls))
        (\y -> OptMonad_Coq.ret (ConcreteEvidence.Coq_ppc x y)))}

