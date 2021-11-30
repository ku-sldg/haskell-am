{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module StVM_Deriving where

--import Term_Defs(RawEv(..))
import StVM(Coq_cvm_st(..))
import MonadCop(Cop_Env(..))
--import Term_Defs_Deriving



--deriving instance Show (TMVar RawEv)
deriving instance Show Coq_cvm_st
deriving instance Show Cop_Env
