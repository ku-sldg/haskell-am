{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Cvm_St_Deriving where

import Cvm_St(Coq_cvm_st(..))
import MonadCop(Cop_Env(..))


deriving instance Show Coq_cvm_st
deriving instance Show Cop_Env
