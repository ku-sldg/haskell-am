module Cvm_Monad where

import qualified Prelude
import qualified Axioms_Io
import qualified ConcreteEvidence
import qualified Cvm_St
import qualified Datatypes
import qualified Evidence_Bundlers
import qualified IO_Stubs
import qualified StMonad_Coq
import qualified Term_Defs

put_ev :: Term_Defs.EvC -> Cvm_St.CVM ()
put_ev e =
  StMonad_Coq.bind StMonad_Coq.get (\st ->
    let {tr' = Cvm_St.st_trace st} in
    let {p' = Cvm_St.st_pl st} in
    let {i = Cvm_St.st_evid st} in
    StMonad_Coq.put (Cvm_St.Coq_mk_st e tr' p' i))

get_ev :: Cvm_St.CVM Term_Defs.EvC
get_ev =
  StMonad_Coq.bind StMonad_Coq.get (\st -> StMonad_Coq.ret (Cvm_St.st_ev st))

get_pl :: Cvm_St.CVM Term_Defs.Plc
get_pl =
  StMonad_Coq.bind StMonad_Coq.get (\st -> StMonad_Coq.ret (Cvm_St.st_pl st))

inc_id :: Cvm_St.CVM Term_Defs.Event_ID
inc_id =
  StMonad_Coq.bind StMonad_Coq.get (\st ->
    let {tr' = Cvm_St.st_trace st} in
    let {e' = Cvm_St.st_ev st} in
    let {p' = Cvm_St.st_pl st} in
    let {i = Cvm_St.st_evid st} in
    StMonad_Coq.bind
      (StMonad_Coq.put (Cvm_St.Coq_mk_st e' tr' p'
        ((Prelude.+) i (Prelude.succ 0)))) (\_ -> StMonad_Coq.ret i))

add_trace :: (([]) Term_Defs.Ev) -> Cvm_St.Coq_cvm_st -> Cvm_St.Coq_cvm_st
add_trace tr' pat =
  case pat of {
   Cvm_St.Coq_mk_st e tr p i -> Cvm_St.Coq_mk_st e (Datatypes.app tr tr') p i}

add_tracem :: (([]) Term_Defs.Ev) -> Cvm_St.CVM ()
add_tracem tr =
  StMonad_Coq.modify (add_trace tr)

split_ev :: Term_Defs.Split -> Cvm_St.CVM ((,) Term_Defs.EvC Term_Defs.EvC)
split_ev sp =
  StMonad_Coq.bind get_ev (\e ->
    StMonad_Coq.bind get_pl (\p ->
      StMonad_Coq.bind inc_id (\i ->
        let {e1 = ConcreteEvidence.splitEv_l sp e} in
        let {e2 = ConcreteEvidence.splitEv_r sp e} in
        StMonad_Coq.bind (add_tracem ((:) (Term_Defs.Coq_split i p) ([])))
          (\_ -> StMonad_Coq.ret ((,) e1 e2)))))

tag_ASP :: Term_Defs.ASP_PARAMS -> Term_Defs.Plc -> Term_Defs.EvC ->
           Cvm_St.CVM Term_Defs.Event_ID
tag_ASP params mpl e =
  StMonad_Coq.bind inc_id (\x ->
    StMonad_Coq.bind
      (add_tracem ((:) (Term_Defs.Coq_umeas x mpl params
        (Term_Defs.get_et e)) ([]))) (\_ -> StMonad_Coq.ret x))

invoke_ASP :: Term_Defs.ASP_PARAMS -> Cvm_St.CVM Term_Defs.EvC
invoke_ASP params =
  StMonad_Coq.bind get_ev (\e ->
    StMonad_Coq.bind get_pl (\p ->
      StMonad_Coq.bind (tag_ASP params p e) (\_ ->
        StMonad_Coq.bind (IO_Stubs.do_asp' params (Term_Defs.get_bits e))
          (\bs -> StMonad_Coq.ret (Evidence_Bundlers.cons_uu bs e params p)))))

tag_SIG :: Term_Defs.Plc -> Term_Defs.EvC -> Cvm_St.CVM Term_Defs.Event_ID
tag_SIG p e =
  StMonad_Coq.bind inc_id (\x ->
    StMonad_Coq.bind
      (add_tracem ((:) (Term_Defs.Coq_sign x p (Term_Defs.get_et e)) ([])))
      (\_ -> StMonad_Coq.ret x))

signEv :: Cvm_St.CVM Term_Defs.EvC
signEv =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind get_ev (\e ->
      StMonad_Coq.bind (tag_SIG p e) (\_ ->
        StMonad_Coq.bind
          (IO_Stubs.do_sig' (Evidence_Bundlers.encodeEvBits e)) (\bs ->
          StMonad_Coq.ret (Evidence_Bundlers.cons_sig bs e p)))))

tag_HSH :: Term_Defs.Plc -> Term_Defs.EvC -> Cvm_St.CVM Term_Defs.Event_ID
tag_HSH p e =
  StMonad_Coq.bind inc_id (\x ->
    StMonad_Coq.bind
      (add_tracem ((:) (Term_Defs.Coq_hash x p (Term_Defs.get_et e)) ([])))
      (\_ -> StMonad_Coq.ret x))

hashEv :: Cvm_St.CVM Term_Defs.EvC
hashEv =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind get_ev (\e ->
      StMonad_Coq.bind (tag_HSH p e) (\_ ->
        StMonad_Coq.bind
          (IO_Stubs.do_hash' (Evidence_Bundlers.encodeEvBits e)) (\bs ->
          StMonad_Coq.ret (Evidence_Bundlers.cons_hh bs e p)))))

copyEv :: Cvm_St.CVM Term_Defs.EvC
copyEv =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind inc_id (\x ->
      StMonad_Coq.bind (add_tracem ((:) (Term_Defs.Coq_copy x p) ([])))
        (\_ -> get_ev)))

do_prim :: Term_Defs.ASP -> Cvm_St.CVM Term_Defs.EvC
do_prim a =
  case a of {
   Term_Defs.CPY -> copyEv;
   Term_Defs.ASPC params -> invoke_ASP params;
   Term_Defs.SIG -> signEv;
   Term_Defs.HSH -> hashEv}

event_id_span :: Term_Defs.Term -> Prelude.Int
event_id_span t =
  case t of {
   Term_Defs.Coq_asp _ -> Prelude.succ 0;
   Term_Defs.Coq_att _ x ->
    (Prelude.+) (Prelude.succ (Prelude.succ 0)) (event_id_span x);
   Term_Defs.Coq_lseq x y -> (Prelude.+) (event_id_span x) (event_id_span y);
   Term_Defs.Coq_bseq _ x y ->
    (Prelude.+)
      ((Prelude.+) (Prelude.succ (Prelude.succ 0)) (event_id_span x))
      (event_id_span y);
   Term_Defs.Coq_bpar _ x y ->
    (Prelude.+)
      ((Prelude.+) (Prelude.succ (Prelude.succ 0)) (event_id_span x))
      (event_id_span y)}

inc_remote_event_ids :: Term_Defs.Term -> Cvm_St.CVM ()
inc_remote_event_ids t =
  StMonad_Coq.bind StMonad_Coq.get (\st ->
    let {tr' = Cvm_St.st_trace st} in
    let {e' = Cvm_St.st_ev st} in
    let {p' = Cvm_St.st_pl st} in
    let {i = Cvm_St.st_evid st} in
    let {new_i = (Prelude.+) i (event_id_span t)} in
    StMonad_Coq.put (Cvm_St.Coq_mk_st e' tr' p' new_i))

tag_REQ :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.Plc -> Term_Defs.EvC
           -> Cvm_St.CVM ()
tag_REQ t p q e =
  StMonad_Coq.bind inc_id (\reqi ->
    add_tracem ((:) (Term_Defs.Coq_req reqi p q t (Term_Defs.get_et e)) ([])))

tag_RPY :: Term_Defs.Plc -> Term_Defs.Plc -> Term_Defs.EvC -> Cvm_St.CVM ()
tag_RPY p q e =
  StMonad_Coq.bind inc_id (\rpyi ->
    add_tracem ((:) (Term_Defs.Coq_rpy rpyi p q (Term_Defs.get_et e)) ([])))

remote_session :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.Plc ->
                  Term_Defs.EvC -> Cvm_St.CVM Term_Defs.EvC
remote_session t p q e =
  StMonad_Coq.bind (tag_REQ t p q e) (\_ ->
    StMonad_Coq.bind (IO_Stubs.doRemote_session' t q e) (\e' ->
      StMonad_Coq.bind
        (add_tracem (Axioms_Io.cvm_events t q (Term_Defs.get_et e))) (\_ ->
        StMonad_Coq.bind (inc_remote_event_ids t) (\_ -> StMonad_Coq.ret e'))))

doRemote :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.EvC -> Cvm_St.CVM
            Term_Defs.EvC
doRemote t q e =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind (remote_session t p q e) (\e' ->
      StMonad_Coq.bind (tag_RPY p q e') (\_ -> StMonad_Coq.ret e')))

join_seq :: Term_Defs.EvC -> Term_Defs.EvC -> Cvm_St.CVM ()
join_seq e1 e2 =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind inc_id (\n ->
      StMonad_Coq.bind (put_ev (Evidence_Bundlers.ss_cons e1 e2)) (\_ ->
        add_tracem ((:) (Term_Defs.Coq_join n p) ([])))))

start_par_thread :: Term_Defs.Loc -> Term_Defs.Term -> Term_Defs.EvC ->
                    Cvm_St.CVM ()
start_par_thread loc t e =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind
      (IO_Stubs.do_start_par_thread loc t (Term_Defs.get_bits e)) (\_ ->
      add_tracem ((:) (Term_Defs.Coq_cvm_thread_start loc p t
        (Term_Defs.get_et e)) ([]))))

wait_par_thread :: Term_Defs.Loc -> Term_Defs.Term -> Term_Defs.EvC ->
                   Cvm_St.CVM Term_Defs.EvC
wait_par_thread loc t _ =
  StMonad_Coq.bind get_pl (\_ ->
    StMonad_Coq.bind (IO_Stubs.do_wait_par_thread loc) (\e' ->
      StMonad_Coq.bind
        (add_tracem ((:) (Term_Defs.Coq_cvm_thread_end loc) ([]))) (\_ ->
        StMonad_Coq.bind (inc_remote_event_ids t) (\_ -> StMonad_Coq.ret e'))))

join_par :: Term_Defs.EvC -> Term_Defs.EvC -> Cvm_St.CVM ()
join_par e1 e2 =
  StMonad_Coq.bind get_pl (\p ->
    StMonad_Coq.bind inc_id (\n ->
      StMonad_Coq.bind (put_ev (Evidence_Bundlers.pp_cons e1 e2)) (\_ ->
        add_tracem ((:) (Term_Defs.Coq_join n p) ([])))))

