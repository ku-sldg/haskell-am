{-# LANGUAGE StandaloneDeriving #-}

module Term_Defs_Deriving where

import Term_Defs
import StVM

deriving instance Show ASP_PARAMS
deriving instance Show ASP
deriving instance Show SP
deriving instance Show Term
deriving instance Show Evidence
deriving instance Show EvC
deriving instance Show Ev
deriving instance Show AnnoTermPar

deriving instance Show Coq_cvm_st

