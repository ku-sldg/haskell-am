module Impl_VM_Extracted where

import qualified Term_Defs (Term, AnnoTermPar, annotated_par, anno_par, RawEv, get_bits, top_level_thread_count, Loc)
import qualified StVM
import qualified GenStMonad
import Control.Monad.IO.Class (liftIO)
import MonadCop (COP, Cop_Env, runCOP, parServer)
import Impl_VM (copland_compile) --hiding (run_cvm)
import Control.Monad.Reader(runReaderT)
import CommTypes
import CommUtil

run_cvm :: Term_Defs.AnnoTermPar -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm t st env =
  runCOP (GenStMonad.execSt (copland_compile t) st) env

run_cvm' :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm' t st env = run_cvm (Term_Defs.annotated_par t) st env


get_term_loc_init :: Term_Defs.Term -> Cop_Env -> IO Term_Defs.Loc
get_term_loc_init t env = do
  let tsize = Term_Defs.top_level_thread_count t
      par_addr = parServer env
      req_msg = ParInit $ InitMessagePar tsize
  putStrLn $ "sending InitMessagePar to: " ++ (show par_addr)
  resp@(AckInitMessagePar new_loc) <- gen_run_client par_addr req_msg
  putStrLn $ "received AckMessagePar: " ++ (show resp)
  return new_loc
  
  

run_cvm_loc' :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO StVM.Coq_cvm_st
run_cvm_loc' t st env = do
  loc <- get_term_loc_init t env
  run_cvm (snd (Term_Defs.anno_par t loc)) st env

run_cvm_loc :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO Term_Defs.RawEv
run_cvm_loc t st env = do
    res_st <- run_cvm_loc' t st env
    return (Term_Defs.get_bits (StVM.st_ev res_st))

run_cvm_rawev :: Term_Defs.Term -> StVM.Coq_cvm_st -> Cop_Env -> IO Term_Defs.RawEv
run_cvm_rawev t st env = do
  res_st <- run_cvm' t st env
  return (Term_Defs.get_bits (StVM.st_ev res_st))
