module Appraisal_Defs where

import qualified Prelude
import qualified BS
import qualified OptMonad_Coq
import qualified Term_Defs

checkHash :: Term_Defs.Evidence -> Term_Defs.Plc -> BS.BS -> OptMonad_Coq.Opt
             BS.BS
checkHash e p hash =
  case e of {
   Term_Defs.Coq_uu _ _ e' -> checkHash e' p hash;
   Term_Defs.Coq_gg _ _ -> OptMonad_Coq.failm;
   Term_Defs.Coq_hh _ e' -> checkHash e' p hash;
   Term_Defs.Coq_ss e1 e2 ->
    OptMonad_Coq.bind (checkHash e1 p hash) (\_ ->
      OptMonad_Coq.bind (checkHash e2 p hash) (\_ ->
        OptMonad_Coq.ret BS.default_bs));
   Term_Defs.Coq_pp e1 e2 ->
    OptMonad_Coq.bind (checkHash e1 p hash) (\_ ->
      OptMonad_Coq.bind (checkHash e2 p hash) (\_ ->
        OptMonad_Coq.ret BS.default_bs));
   _ -> OptMonad_Coq.ret BS.default_bs}

