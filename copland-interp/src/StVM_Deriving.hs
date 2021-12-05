{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module StVM_Deriving where

import StVM(Coq_cvm_st(..))
import MonadCop(Cop_Env(..))


deriving instance Show Coq_cvm_st
deriving instance Show Cop_Env
