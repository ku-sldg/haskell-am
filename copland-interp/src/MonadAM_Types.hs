module MonadAM_Types where

--import Copland


import BS

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

{-  The Attestation Manager Monad  -}
--type AM = ReaderT AM_Env (StateT AM_St IO)
type AM = StateT AM_St (ReaderT AM_Env IO)


--type AM = ReaderT AM_Env (StateT AM_St (MaybeT IO))

{-  Read-only environment used during protocol execution.
    Contains:

-}

data AM_Env =
  AM_Env { sig_map :: M.Map Int Int,
           hsh_map :: M.Map Int Int,
           asp_map :: M.Map (Int,Int) Int
           {-nonce_check_asp :: Int -}
          } deriving (Show)

type Policy = String

{-  State for state monad.
    Contains:
 -}
data AM_St =
  AM_St { am_nonceMap :: M.Map Int BS,
          {-am_nonceAppraiseMap :: M.Map Int Bool, -}
          am_nonceId  :: Int
         } deriving (Show)


empty_AM_state = AM_St { am_nonceMap =  M.empty,
                         {-am_nonceAppraiseMap = M.empty, -}
                         am_nonceId = 0 }

empty_AM_env = AM_Env {  sig_map = M.empty,
                         hsh_map = M.empty,
                         asp_map = M.empty
                         {-nonce_check_asp = 0-} }

initial_AM_env sigMap hshMap aspMap =
  empty_AM_env { sig_map = sigMap, hsh_map = hshMap, asp_map = aspMap
                   {-nonce_check_asp = nca-} }

runAM :: AM a -> AM_Env -> AM_St -> IO (a, AM_St)
runAM k env st =
     --runStateT (runReaderT k env) st
  runReaderT (runStateT k st) env








{-



data AM_Env =
  AM_Env { sig_map :: M.Map Plc ASP_ID,
           hsh_map :: M.Map Plc ASP_ID,
           asp_map :: M.Map (Plc,ASP_ID) ASP_ID
           {-nonce_check_asp :: ASP_ID -}
          } deriving (Show)

type Policy = String

{-  State for state monad.
    Contains:
 -}
data AM_St =
  AM_St { am_nonceMap :: M.Map Int BS,
          {-am_nonceAppraiseMap :: M.Map Int Bool, -}
          am_nonceId  :: Int
         } deriving (Show)



-}
