module Evidence_Bundlers where

import qualified Prelude
import qualified BS
import qualified Datatypes
import qualified IO_Stubs
import qualified Term_Defs

cons_uu :: BS.BS -> Term_Defs.EvC -> Term_Defs.ASP_PARAMS -> Term_Defs.Plc ->
           Term_Defs.EvC
cons_uu x e params mpl =
  case e of {
   Term_Defs.Coq_evc bits et -> Term_Defs.Coq_evc ((:) x bits)
    (Term_Defs.Coq_uu params mpl et)}

cons_sig :: BS.BS -> Term_Defs.EvC -> Term_Defs.Plc -> Term_Defs.EvC
cons_sig sig e p =
  case e of {
   Term_Defs.Coq_evc bits et -> Term_Defs.Coq_evc ((:) sig bits)
    (Term_Defs.Coq_gg p et)}

cons_hh :: BS.BS -> Term_Defs.EvC -> Term_Defs.Plc -> Term_Defs.EvC
cons_hh hsh e p =
  case e of {
   Term_Defs.Coq_evc _ et -> Term_Defs.Coq_evc ((:) hsh ([]))
    (Term_Defs.Coq_hh p et)}

ss_cons :: Term_Defs.EvC -> Term_Defs.EvC -> Term_Defs.EvC
ss_cons e1 e2 =
  case e1 of {
   Term_Defs.Coq_evc bits1 et1 ->
    case e2 of {
     Term_Defs.Coq_evc bits2 et2 -> Term_Defs.Coq_evc
      (Datatypes.app bits1 bits2) (Term_Defs.Coq_ss et1 et2)}}

pp_cons :: Term_Defs.EvC -> Term_Defs.EvC -> Term_Defs.EvC
pp_cons e1 e2 =
  case e1 of {
   Term_Defs.Coq_evc bits1 et1 ->
    case e2 of {
     Term_Defs.Coq_evc bits2 et2 -> Term_Defs.Coq_evc
      (Datatypes.app bits1 bits2) (Term_Defs.Coq_pp et1 et2)}}

encodeEvBits :: Term_Defs.EvC -> BS.BS
encodeEvBits e =
  case e of {
   Term_Defs.Coq_evc bits _ -> IO_Stubs.encodeEvRaw bits}

