module MonadVM where

import qualified Prelude
import qualified Axioms_Io
import qualified ConcreteEvidence
import qualified Datatypes
import qualified Evidence_Bundlers
import qualified GenStMonad
import qualified IO_Stubs
import qualified StVM
import qualified Term_Defs

put_ev :: Term_Defs.EvC -> StVM.CVM ()
put_ev e =
  GenStMonad.bind GenStMonad.get (\st ->
    let {tr' = StVM.st_trace st} in
    let {p' = StVM.st_pl st} in
    let {i = StVM.st_evid st} in GenStMonad.put (StVM.Coq_mk_st e tr' p' i))

get_ev :: StVM.CVM Term_Defs.EvC
get_ev =
  GenStMonad.bind GenStMonad.get (\st -> GenStMonad.ret (StVM.st_ev st))

get_pl :: StVM.CVM Term_Defs.Plc
get_pl =
  GenStMonad.bind GenStMonad.get (\st -> GenStMonad.ret (StVM.st_pl st))

inc_id :: StVM.CVM Term_Defs.Event_ID
inc_id =
  GenStMonad.bind GenStMonad.get (\st ->
    let {tr' = StVM.st_trace st} in
    let {e' = StVM.st_ev st} in
    let {p' = StVM.st_pl st} in
    let {i = StVM.st_evid st} in
    GenStMonad.bind
      (GenStMonad.put (StVM.Coq_mk_st e' tr' p'
        ((Prelude.+) i (Prelude.succ 0)))) (\_ -> GenStMonad.ret i))

add_trace :: (([]) Term_Defs.Ev) -> StVM.Coq_cvm_st -> StVM.Coq_cvm_st
add_trace tr' pat =
  case pat of {
   StVM.Coq_mk_st e tr p i -> StVM.Coq_mk_st e (Datatypes.app tr tr') p i}

add_tracem :: (([]) Term_Defs.Ev) -> StVM.CVM ()
add_tracem tr =
  GenStMonad.modify (add_trace tr)

split_ev :: Term_Defs.Split -> StVM.CVM ((,) Term_Defs.EvC Term_Defs.EvC)
split_ev sp =
  GenStMonad.bind get_ev (\e ->
    GenStMonad.bind get_pl (\p ->
      GenStMonad.bind inc_id (\i ->
        let {e1 = ConcreteEvidence.splitEv_l sp e} in
        let {e2 = ConcreteEvidence.splitEv_r sp e} in
        GenStMonad.bind (add_tracem ((:) (Term_Defs.Coq_split i p) ([])))
          (\_ -> GenStMonad.ret ((,) e1 e2)))))

tag_ASP :: Term_Defs.ASP_PARAMS -> Term_Defs.Plc -> Term_Defs.EvC -> StVM.CVM
           Term_Defs.Event_ID
tag_ASP params mpl e =
  GenStMonad.bind inc_id (\x ->
    GenStMonad.bind
      (add_tracem ((:) (Term_Defs.Coq_umeas x mpl params
        (Term_Defs.get_et e)) ([]))) (\_ -> GenStMonad.ret x))

invoke_ASP :: Term_Defs.ASP_PARAMS -> StVM.CVM Term_Defs.EvC
invoke_ASP params =
  GenStMonad.bind get_ev (\e ->
    GenStMonad.bind get_pl (\p ->
      GenStMonad.bind (tag_ASP params p e) (\_ ->
        GenStMonad.bind (IO_Stubs.do_asp' params (Term_Defs.get_bits e))
          (\bs -> GenStMonad.ret (Evidence_Bundlers.cons_uu bs e params p)))))

tag_SIG :: Term_Defs.Plc -> Term_Defs.EvC -> StVM.CVM Term_Defs.Event_ID
tag_SIG p e =
  GenStMonad.bind inc_id (\x ->
    GenStMonad.bind
      (add_tracem ((:) (Term_Defs.Coq_sign x p (Term_Defs.get_et e)) ([])))
      (\_ -> GenStMonad.ret x))

signEv :: StVM.CVM Term_Defs.EvC
signEv =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind get_ev (\e ->
      GenStMonad.bind (tag_SIG p e) (\_ ->
        GenStMonad.bind (IO_Stubs.do_sig' (Evidence_Bundlers.encodeEvBits e))
          (\bs -> GenStMonad.ret (Evidence_Bundlers.cons_sig bs e p)))))

tag_HSH :: Term_Defs.Plc -> Term_Defs.EvC -> StVM.CVM Term_Defs.Event_ID
tag_HSH p e =
  GenStMonad.bind inc_id (\x ->
    GenStMonad.bind
      (add_tracem ((:) (Term_Defs.Coq_hash x p (Term_Defs.get_et e)) ([])))
      (\_ -> GenStMonad.ret x))

hashEv :: StVM.CVM Term_Defs.EvC
hashEv =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind get_ev (\e ->
      GenStMonad.bind (tag_HSH p e) (\_ ->
        GenStMonad.bind
          (IO_Stubs.do_hash' (Evidence_Bundlers.encodeEvBits e)) (\bs ->
          GenStMonad.ret (Evidence_Bundlers.cons_hh bs e p)))))

copyEv :: StVM.CVM Term_Defs.EvC
copyEv =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind inc_id (\x ->
      GenStMonad.bind (add_tracem ((:) (Term_Defs.Coq_copy x p) ([]))) (\_ ->
        get_ev)))

do_prim :: Term_Defs.ASP -> StVM.CVM Term_Defs.EvC
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

inc_remote_event_ids :: Term_Defs.Term -> StVM.CVM ()
inc_remote_event_ids t =
  GenStMonad.bind GenStMonad.get (\st ->
    let {tr' = StVM.st_trace st} in
    let {e' = StVM.st_ev st} in
    let {p' = StVM.st_pl st} in
    let {i = StVM.st_evid st} in
    let {new_i = (Prelude.+) i (event_id_span t)} in
    GenStMonad.put (StVM.Coq_mk_st e' tr' p' new_i))

tag_REQ :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.Plc -> Term_Defs.EvC
           -> StVM.CVM ()
tag_REQ t p q e =
  GenStMonad.bind inc_id (\reqi ->
    add_tracem ((:) (Term_Defs.Coq_req reqi p q t (Term_Defs.get_et e)) ([])))

tag_RPY :: Term_Defs.Plc -> Term_Defs.Plc -> Term_Defs.EvC -> StVM.CVM ()
tag_RPY p q e =
  GenStMonad.bind inc_id (\rpyi ->
    add_tracem ((:) (Term_Defs.Coq_rpy rpyi p q (Term_Defs.get_et e)) ([])))

remote_session :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.Plc ->
                  Term_Defs.EvC -> StVM.CVM Term_Defs.EvC
remote_session t p q e =
  GenStMonad.bind (tag_REQ t p q e) (\_ ->
    GenStMonad.bind (IO_Stubs.doRemote_session' t q e) (\e' ->
      GenStMonad.bind
        (add_tracem (Axioms_Io.cvm_events t q (Term_Defs.get_et e))) (\_ ->
        GenStMonad.bind (inc_remote_event_ids t) (\_ -> GenStMonad.ret e'))))

doRemote :: Term_Defs.Term -> Term_Defs.Plc -> Term_Defs.EvC -> StVM.CVM
            Term_Defs.EvC
doRemote t q e =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind (remote_session t p q e) (\e' ->
      GenStMonad.bind (tag_RPY p q e') (\_ -> GenStMonad.ret e')))

join_seq :: Term_Defs.EvC -> Term_Defs.EvC -> StVM.CVM ()
join_seq e1 e2 =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind inc_id (\n ->
      GenStMonad.bind (put_ev (Evidence_Bundlers.ss_cons e1 e2)) (\_ ->
        add_tracem ((:) (Term_Defs.Coq_join n p) ([])))))

start_par_thread :: Term_Defs.Loc -> Term_Defs.Term -> Term_Defs.EvC ->
                    StVM.CVM ()
start_par_thread loc t e =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind
      (IO_Stubs.do_start_par_thread loc t (Term_Defs.get_bits e)) (\_ ->
      add_tracem ((:) (Term_Defs.Coq_cvm_thread_start loc p t
        (Term_Defs.get_et e)) ([]))))

wait_par_thread :: Term_Defs.Loc -> Term_Defs.Term -> Term_Defs.EvC ->
                   StVM.CVM Term_Defs.EvC
wait_par_thread loc t _ =
  GenStMonad.bind get_pl (\_ ->
    GenStMonad.bind (IO_Stubs.do_wait_par_thread loc) (\e' ->
      GenStMonad.bind
        (add_tracem ((:) (Term_Defs.Coq_cvm_thread_end loc) ([]))) (\_ ->
        GenStMonad.bind (inc_remote_event_ids t) (\_ -> GenStMonad.ret e'))))

join_par :: Term_Defs.EvC -> Term_Defs.EvC -> StVM.CVM ()
join_par e1 e2 =
  GenStMonad.bind get_pl (\p ->
    GenStMonad.bind inc_id (\n ->
      GenStMonad.bind (put_ev (Evidence_Bundlers.pp_cons e1 e2)) (\_ ->
        add_tracem ((:) (Term_Defs.Coq_join n p) ([])))))

