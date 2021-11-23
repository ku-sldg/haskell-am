module Term_Defs where

import qualified Prelude
import qualified BS

type Plc = Prelude.Int

type N_ID = Prelude.Int

type Event_ID = Prelude.Int

type ASP_ID = Prelude.Int

type TARG_ID = Prelude.Int

type Arg = Prelude.String

data ASP_PARAMS =
   Coq_asp_paramsC ASP_ID (([]) Arg) Plc TARG_ID

data ASP =
   CPY
 | ASPC ASP_PARAMS
 | SIG
 | HSH

data SP =
   ALL
 | NONE

type Split = (,) SP SP

data Term =
   Coq_asp ASP
 | Coq_att Plc Term
 | Coq_lseq Term Term
 | Coq_bseq Split Term Term
 | Coq_bpar Split Term Term

data Evidence =
   Coq_mt
 | Coq_uu ASP_PARAMS Plc Evidence
 | Coq_gg Plc Evidence
 | Coq_hh Plc Evidence
 | Coq_nn N_ID
 | Coq_ss Evidence Evidence
 | Coq_pp Evidence Evidence

type RawEv = ([]) BS.BS

data EvC =
   Coq_evc RawEv Evidence

mt_evc :: EvC
mt_evc =
  Coq_evc ([]) Coq_mt

get_et :: EvC -> Evidence
get_et e =
  case e of {
   Coq_evc _ et -> et}

get_bits :: EvC -> ([]) BS.BS
get_bits e =
  case e of {
   Coq_evc ls _ -> ls}

type Loc = Prelude.Int

data Ev =
   Coq_copy Prelude.Int Plc
 | Coq_umeas Prelude.Int Plc ASP_PARAMS
 | Coq_sign Prelude.Int Plc Evidence
 | Coq_hash Prelude.Int Plc Evidence
 | Coq_req Prelude.Int Plc Plc Term Evidence
 | Coq_rpy Prelude.Int Plc Plc Evidence
 | Coq_split Prelude.Int Plc
 | Coq_join Prelude.Int Plc
 | Coq_cvm_thread_start Loc Plc Term Evidence
 | Coq_cvm_thread_end Loc

data AnnoTermPar =
   Coq_aasp_par ASP
 | Coq_aatt_par Plc Term
 | Coq_alseq_par AnnoTermPar AnnoTermPar
 | Coq_abseq_par Split AnnoTermPar AnnoTermPar
 | Coq_abpar_par Loc Split AnnoTermPar Term

