module StVM where

import qualified Prelude
import qualified GenStMonad
import qualified Term_Defs

data Coq_cvm_st =
   Coq_mk_st Term_Defs.EvC (([]) Term_Defs.Ev) Term_Defs.Plc Term_Defs.Event_ID

st_ev :: Coq_cvm_st -> Term_Defs.EvC
st_ev c =
  case c of {
   Coq_mk_st st_ev0 _ _ _ -> st_ev0}

st_trace :: Coq_cvm_st -> ([]) Term_Defs.Ev
st_trace c =
  case c of {
   Coq_mk_st _ st_trace0 _ _ -> st_trace0}

st_pl :: Coq_cvm_st -> Term_Defs.Plc
st_pl c =
  case c of {
   Coq_mk_st _ _ st_pl0 _ -> st_pl0}

st_evid :: Coq_cvm_st -> Term_Defs.Event_ID
st_evid c =
  case c of {
   Coq_mk_st _ _ _ st_evid0 -> st_evid0}

type CVM a = GenStMonad.St Coq_cvm_st a

