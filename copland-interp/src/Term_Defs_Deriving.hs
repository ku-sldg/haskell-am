{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Term_Defs_Deriving where

import Term_Defs
--import StVM

import GHC.Generics (Generic)

import qualified Data.Binary as BI (Binary)

deriving instance Show ASP_PARAMS
deriving instance Show ASP
deriving instance Show SP
deriving instance Show Term
deriving instance Show Evidence
deriving instance Show EvC
deriving instance Show Ev
deriving instance Show AnnoTermPar

deriving instance Read ASP_PARAMS
deriving instance Read ASP
deriving instance Read SP
deriving instance Read Term
deriving instance Read Evidence
deriving instance Read EvC
deriving instance Read Ev
deriving instance Read AnnoTermPar

deriving instance Generic Evidence
deriving instance Generic ASP
deriving instance Generic ASP_PARAMS
deriving instance Generic SP
deriving instance Generic Term

--deriving instance Show Coq_cvm_st

instance BI.Binary SP where
instance BI.Binary ASP_PARAMS where
instance BI.Binary ASP where
instance BI.Binary Evidence where
