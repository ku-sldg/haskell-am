module Impl_appraisal_alt where

import qualified Prelude
import qualified Appraisal_Defs
import qualified Appraisal_Evidence
import qualified Appraisal_IO_Stubs
import qualified ConcreteEvidence
import qualified GenOptMonad
import qualified List
import qualified Term_Defs

build_app_comp_evC :: Term_Defs.Evidence -> Term_Defs.RawEv -> GenOptMonad.AM
                      ConcreteEvidence.EvidenceC
build_app_comp_evC et ls =
  case et of {
   Term_Defs.Coq_mt -> GenOptMonad.ret ConcreteEvidence.Coq_mtc;
   Term_Defs.Coq_uu params p et' ->
    GenOptMonad.bind (Appraisal_Evidence.peel_bs ls) (\x0 ->
      case x0 of {
       (,) bs ls' ->
        GenOptMonad.bind (build_app_comp_evC et' ls') (\x ->
          GenOptMonad.bind (Appraisal_IO_Stubs.checkASP params bs) (\res ->
            GenOptMonad.ret (ConcreteEvidence.Coq_uuc params p res x)))});
   Term_Defs.Coq_gg p et' ->
    GenOptMonad.bind (Appraisal_Evidence.peel_bs ls) (\x0 ->
      case x0 of {
       (,) bs ls' ->
        GenOptMonad.bind (build_app_comp_evC et' ls') (\x ->
          GenOptMonad.bind (Appraisal_IO_Stubs.checkSigBits ls' p bs)
            (\res -> GenOptMonad.ret (ConcreteEvidence.Coq_ggc p res x)))});
   Term_Defs.Coq_hh p et0 ->
    GenOptMonad.bind (Appraisal_Evidence.peel_bs ls) (\x ->
      case x of {
       (,) bs _ ->
        GenOptMonad.bind (Appraisal_Defs.checkHash et0 p bs) (\res ->
          GenOptMonad.ret (ConcreteEvidence.Coq_hhc p res et0))});
   Term_Defs.Coq_nn nid ->
    GenOptMonad.bind (Appraisal_Evidence.peel_bs ls) (\x ->
      case x of {
       (,) bs _ ->
        GenOptMonad.bind (Appraisal_IO_Stubs.checkNonce nid bs) (\res ->
          GenOptMonad.ret (ConcreteEvidence.Coq_nnc nid res))});
   Term_Defs.Coq_ss et1 et2 ->
    GenOptMonad.bind
      (build_app_comp_evC et1 (List.firstn (Term_Defs.et_size et1) ls))
      (\x ->
      GenOptMonad.bind
        (build_app_comp_evC et2 (List.skipn (Term_Defs.et_size et1) ls))
        (\y -> GenOptMonad.ret (ConcreteEvidence.Coq_ssc x y)));
   Term_Defs.Coq_pp et1 et2 ->
    GenOptMonad.bind
      (build_app_comp_evC et1 (List.firstn (Term_Defs.et_size et1) ls))
      (\x ->
      GenOptMonad.bind
        (build_app_comp_evC et2 (List.skipn (Term_Defs.et_size et1) ls))
        (\y -> GenOptMonad.ret (ConcreteEvidence.Coq_ppc x y)))}

