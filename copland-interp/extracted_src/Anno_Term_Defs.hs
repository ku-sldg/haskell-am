module Anno_Term_Defs where

import qualified Prelude
import qualified Datatypes
import qualified GenOptMonad
import qualified Term_Defs

data AnnoTermPar =
   Coq_aasp_par Term_Defs.ASP
 | Coq_aatt_par Term_Defs.Plc Term_Defs.Term
 | Coq_alseq_par AnnoTermPar AnnoTermPar
 | Coq_abseq_par Term_Defs.Split AnnoTermPar AnnoTermPar
 | Coq_abpar_par Term_Defs.Loc Term_Defs.Split AnnoTermPar Term_Defs.Term

anno_par :: Term_Defs.Term -> Term_Defs.Loc -> (,) Term_Defs.Loc AnnoTermPar
anno_par t loc =
  case t of {
   Term_Defs.Coq_asp a -> (,) loc (Coq_aasp_par a);
   Term_Defs.Coq_att p t' -> (,) loc (Coq_aatt_par p t');
   Term_Defs.Coq_lseq t1 t2 ->
    case anno_par t1 loc of {
     (,) loc' t1' ->
      case anno_par t2 loc' of {
       (,) loc'' t2' -> (,) loc'' (Coq_alseq_par t1' t2')}};
   Term_Defs.Coq_bseq spl t1 t2 ->
    case anno_par t1 loc of {
     (,) loc' t1' ->
      case anno_par t2 loc' of {
       (,) loc'' t2' -> (,) loc'' (Coq_abseq_par spl t1' t2')}};
   Term_Defs.Coq_bpar spl t1 t2 ->
    case anno_par t1 (Prelude.succ loc) of {
     (,) loc' t1' -> (,) loc' (Coq_abpar_par loc spl t1' t2)}}

peel_loc :: (([]) Term_Defs.Loc) -> GenOptMonad.Opt
            ((,) Term_Defs.Loc (([]) Term_Defs.Loc))
peel_loc ls =
  case ls of {
   ([]) -> GenOptMonad.failm;
   (:) bs ls' -> GenOptMonad.ret ((,) bs ls')}

anno_par_list' :: Term_Defs.Term -> (([]) Term_Defs.Loc) -> GenOptMonad.Opt
                  ((,) (([]) Term_Defs.Loc) AnnoTermPar)
anno_par_list' t ls =
  case t of {
   Term_Defs.Coq_asp a -> GenOptMonad.ret ((,) ls (Coq_aasp_par a));
   Term_Defs.Coq_att p t' -> GenOptMonad.ret ((,) ls (Coq_aatt_par p t'));
   Term_Defs.Coq_lseq t1 t2 ->
    GenOptMonad.bind (anno_par_list' t1 ls) (\x ->
      case x of {
       (,) ls' t1' ->
        GenOptMonad.bind (anno_par_list' t2 ls') (\x0 ->
          case x0 of {
           (,) ls'' t2' -> GenOptMonad.ret ((,) ls'' (Coq_alseq_par t1' t2'))})});
   Term_Defs.Coq_bseq spl t1 t2 ->
    GenOptMonad.bind (anno_par_list' t1 ls) (\x ->
      case x of {
       (,) ls' t1' ->
        GenOptMonad.bind (anno_par_list' t2 ls') (\x0 ->
          case x0 of {
           (,) ls'' t2' ->
            GenOptMonad.ret ((,) ls'' (Coq_abseq_par spl t1' t2'))})});
   Term_Defs.Coq_bpar spl t1 t2 ->
    GenOptMonad.bind (peel_loc ls) (\x ->
      case x of {
       (,) loc ls' ->
        GenOptMonad.bind (anno_par_list' t1 ls') (\x0 ->
          case x0 of {
           (,) ls'' t1' ->
            GenOptMonad.ret ((,) ls'' (Coq_abpar_par loc spl t1' t2))})})}

anno_par_list :: Term_Defs.Term -> (([]) Term_Defs.Loc) -> GenOptMonad.Opt
                 AnnoTermPar
anno_par_list t ls =
  GenOptMonad.bind (anno_par_list' t ls) (\x ->
    case x of {
     (,) _ t' -> GenOptMonad.ret t'})

annotated_par :: Term_Defs.Term -> AnnoTermPar
annotated_par x =
  Datatypes.snd (anno_par x 0)

