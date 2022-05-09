module Cvm_Impl where

import qualified Prelude
import qualified Anno_Term_Defs
import qualified Cvm_Monad
import qualified Cvm_St
import qualified StMonad_Coq

copland_compile :: Anno_Term_Defs.AnnoTermPar -> Cvm_St.CVM ()
copland_compile t =
  case t of {
   Anno_Term_Defs.Coq_aasp_par a ->
    StMonad_Coq.bind (Cvm_Monad.do_prim a) Cvm_Monad.put_ev;
   Anno_Term_Defs.Coq_aatt_par q t' ->
    StMonad_Coq.bind Cvm_Monad.get_ev (\e ->
      StMonad_Coq.bind (Cvm_Monad.doRemote t' q e) Cvm_Monad.put_ev);
   Anno_Term_Defs.Coq_alseq_par t1 t2 ->
    StMonad_Coq.bind (copland_compile t1) (\_ -> copland_compile t2);
   Anno_Term_Defs.Coq_abseq_par sp t1 t2 ->
    StMonad_Coq.bind (Cvm_Monad.split_ev sp) (\pr ->
      case pr of {
       (,) e1 e2 ->
        StMonad_Coq.bind (Cvm_Monad.put_ev e1) (\_ ->
          StMonad_Coq.bind (copland_compile t1) (\_ ->
            StMonad_Coq.bind Cvm_Monad.get_ev (\e1r ->
              StMonad_Coq.bind (Cvm_Monad.put_ev e2) (\_ ->
                StMonad_Coq.bind (copland_compile t2) (\_ ->
                  StMonad_Coq.bind Cvm_Monad.get_ev (\e2r ->
                    Cvm_Monad.join_seq e1r e2r))))))});
   Anno_Term_Defs.Coq_abpar_par loc sp t1 t2 ->
    StMonad_Coq.bind (Cvm_Monad.split_ev sp) (\pr ->
      case pr of {
       (,) e1 e2 ->
        StMonad_Coq.bind (Cvm_Monad.start_par_thread loc t2 e2) (\_ ->
          StMonad_Coq.bind (Cvm_Monad.put_ev e1) (\_ ->
            StMonad_Coq.bind (copland_compile t1) (\_ ->
              StMonad_Coq.bind Cvm_Monad.get_ev (\e1r ->
                StMonad_Coq.bind (Cvm_Monad.wait_par_thread loc t2 e2)
                  (\e2r -> Cvm_Monad.join_par e1r e2r)))))})}

