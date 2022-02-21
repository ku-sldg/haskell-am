module Example_Phrases where

import qualified Prelude
import qualified Example_Phrases_Admits
import qualified Term_Defs

coq_P0 :: Term_Defs.Plc
coq_P0 =
  0

coq_P1 :: Term_Defs.Plc
coq_P1 =
  Prelude.succ 0

coq_P2 :: Term_Defs.Plc
coq_P2 =
  Prelude.succ (Prelude.succ 0)

coq_P3 :: Term_Defs.Plc
coq_P3 =
  Prelude.succ (Prelude.succ (Prelude.succ 0))

coq_P4 :: Term_Defs.Plc
coq_P4 =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ 0)))

attest :: Term_Defs.Plc -> Term_Defs.TARG_ID -> Term_Defs.Term
attest p targ =
  Term_Defs.Coq_asp (Term_Defs.ASPC (Term_Defs.Coq_asp_paramsC
    Example_Phrases_Admits.attest_id ([]) p targ))

appraise :: Term_Defs.Plc -> Term_Defs.TARG_ID -> Term_Defs.Term
appraise p targ =
  Term_Defs.Coq_asp (Term_Defs.ASPC (Term_Defs.Coq_asp_paramsC
    Example_Phrases_Admits.appraise_id ([]) p targ))

certificate :: Term_Defs.Plc -> Term_Defs.TARG_ID -> Term_Defs.Term
certificate p targ =
  Term_Defs.Coq_asp (Term_Defs.ASPC (Term_Defs.Coq_asp_paramsC
    Example_Phrases_Admits.cert_id ([]) p targ))

store :: Term_Defs.Plc -> Term_Defs.TARG_ID -> Term_Defs.Term
store p targ =
  Term_Defs.Coq_asp (Term_Defs.ASPC (Term_Defs.Coq_asp_paramsC
    Example_Phrases_Admits.store_id ([]) p targ))

retrieve :: Term_Defs.Plc -> Term_Defs.TARG_ID -> Term_Defs.Term
retrieve p targ =
  Term_Defs.Coq_asp (Term_Defs.ASPC (Term_Defs.Coq_asp_paramsC
    Example_Phrases_Admits.retrieve_id ([]) p targ))

cert_style_simple_sig :: Term_Defs.Term
cert_style_simple_sig =
  Term_Defs.Coq_att coq_P1 (Term_Defs.Coq_lseq
    (attest coq_P1 Example_Phrases_Admits.sys) (Term_Defs.Coq_att
    (Prelude.succ (Prelude.succ 0)) (Term_Defs.Coq_lseq
    (appraise coq_P2 Example_Phrases_Admits.sys) (Term_Defs.Coq_asp
    Term_Defs.SIG))))

cert_style :: Term_Defs.Term
cert_style =
  Term_Defs.Coq_att coq_P1 (Term_Defs.Coq_lseq
    (attest coq_P1 Example_Phrases_Admits.sys) (Term_Defs.Coq_att
    (Prelude.succ (Prelude.succ 0)) (Term_Defs.Coq_lseq
    (appraise coq_P2 Example_Phrases_Admits.sys)
    (certificate coq_P2 Example_Phrases_Admits.sys))))

cert_cache_p1 :: Term_Defs.Term
cert_cache_p1 =
  Term_Defs.Coq_lseq (attest coq_P1 Example_Phrases_Admits.sys)
    (Term_Defs.Coq_lseq (Term_Defs.Coq_att coq_P2 (Term_Defs.Coq_lseq
    (appraise coq_P2 Example_Phrases_Admits.sys)
    (certificate coq_P2 Example_Phrases_Admits.sys)))
    (store coq_P1 Example_Phrases_Admits.cache))

cert_cache_p0 :: Term_Defs.Term
cert_cache_p0 =
  Term_Defs.Coq_att coq_P1 (Term_Defs.Coq_lseq (Term_Defs.Coq_bseq ((,)
    Term_Defs.NONE Term_Defs.ALL)
    (retrieve coq_P1 Example_Phrases_Admits.cache) (Term_Defs.Coq_asp
    Term_Defs.CPY)) (Term_Defs.Coq_asp Term_Defs.SIG))

bg_check :: Term_Defs.Term
bg_check =
  Term_Defs.Coq_lseq (Term_Defs.Coq_att coq_P1
    (attest coq_P1 Example_Phrases_Admits.sys)) (Term_Defs.Coq_att coq_P2
    (appraise coq_P2 Example_Phrases_Admits.sys))

par_mut_p0 :: Term_Defs.Term
par_mut_p0 =
  Term_Defs.Coq_lseq (Term_Defs.Coq_att coq_P1
    (attest coq_P1 Example_Phrases_Admits.sys)) (Term_Defs.Coq_att coq_P2
    (appraise coq_P2 Example_Phrases_Admits.sys))

par_mut_p1 :: Term_Defs.Term
par_mut_p1 =
  Term_Defs.Coq_lseq (Term_Defs.Coq_att coq_P0
    (attest coq_P0 Example_Phrases_Admits.sys)) (Term_Defs.Coq_att coq_P2
    (appraise coq_P2 Example_Phrases_Admits.sys))

layered_bg' :: Term_Defs.Term
layered_bg' =
  Term_Defs.Coq_lseq (attest coq_P1 Example_Phrases_Admits.sys)
    (Term_Defs.Coq_lseq (attest coq_P3 Example_Phrases_Admits.att_tid)
    (attest coq_P4 Example_Phrases_Admits.att_tid))

layered_bg'' :: Term_Defs.Term
layered_bg'' =
  Term_Defs.Coq_bpar ((,) Term_Defs.ALL Term_Defs.ALL) (Term_Defs.Coq_att
    coq_P3 (attest coq_P3 Example_Phrases_Admits.sys)) (Term_Defs.Coq_att
    coq_P4 (attest coq_P4 Example_Phrases_Admits.sys))

layered_bg_weak :: Term_Defs.Term
layered_bg_weak =
  Term_Defs.Coq_att coq_P1 (Term_Defs.Coq_lseq (Term_Defs.Coq_bpar ((,)
    Term_Defs.ALL Term_Defs.ALL) layered_bg' layered_bg'') (Term_Defs.Coq_att
    coq_P2 (Term_Defs.Coq_lseq (appraise coq_P2 Example_Phrases_Admits.it)
    (Term_Defs.Coq_asp Term_Defs.SIG))))

layered_bg_strong :: Term_Defs.Term
layered_bg_strong =
  Term_Defs.Coq_att coq_P1 (Term_Defs.Coq_lseq (Term_Defs.Coq_bseq ((,)
    Term_Defs.ALL Term_Defs.ALL) layered_bg' layered_bg'') (Term_Defs.Coq_att
    coq_P2 (Term_Defs.Coq_lseq (appraise coq_P2 Example_Phrases_Admits.it)
    (Term_Defs.Coq_asp Term_Defs.SIG))))

test_par_nested :: Term_Defs.Term
test_par_nested =
  Term_Defs.Coq_bpar ((,) Term_Defs.ALL Term_Defs.ALL) (Term_Defs.Coq_asp
    Term_Defs.SIG) (Term_Defs.Coq_bpar ((,) Term_Defs.ALL Term_Defs.ALL)
    (Term_Defs.Coq_asp Term_Defs.SIG) (Term_Defs.Coq_asp Term_Defs.SIG))

