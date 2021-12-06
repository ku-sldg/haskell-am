module Appraisal_Defs where

import qualified Prelude
import qualified BS
import qualified GenOptMonad
import qualified Term_Defs

checkHash :: Term_Defs.Evidence -> Term_Defs.Plc -> BS.BS -> GenOptMonad.AM
             BS.BS
checkHash e p hash =
  case e of {
   Term_Defs.Coq_uu _ _ e' -> checkHash e' p hash;
   Term_Defs.Coq_gg _ _ -> GenOptMonad.failm;
   Term_Defs.Coq_hh _ e' -> checkHash e' p hash;
   Term_Defs.Coq_ss e1 e2 ->
    GenOptMonad.bind (checkHash e1 p hash) (\_ ->
      GenOptMonad.bind (checkHash e2 p hash) (\_ ->
        GenOptMonad.ret BS.default_bs));
   Term_Defs.Coq_pp e1 e2 ->
    GenOptMonad.bind (checkHash e1 p hash) (\_ ->
      GenOptMonad.bind (checkHash e2 p hash) (\_ ->
        GenOptMonad.ret BS.default_bs));
   _ -> GenOptMonad.ret BS.default_bs}

