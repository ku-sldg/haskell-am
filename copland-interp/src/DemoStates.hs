{-  Library of initial states for one-off Demos.
    TODO:  Make these more user-facing and externally configurable.

  Author: Adam Petz
  Date:  04/22/2020

-}

{-# LANGUAGE ScopedTypeVariables #-}

module DemoStates where

import Copland
import MonadAM
--import MonadVM_Old
import qualified ServerProgArgs as SA (Server_Options(..))

import qualified Data.Map as M

zero_plc :: Plc
zero_plc = 0

zero_addr :: Address
zero_addr = "CVM0"

one_plc :: Plc
one_plc = 1

one_addr :: Address
one_addr = "CVM1"

two_plc :: Plc
two_plc = 2

two_addr :: Address
two_addr = "CVM2"

sample_client_name_map :: M.Map Plc Address
sample_client_name_map = M.fromList [(zero_plc, zero_addr), (one_plc, one_addr), (two_plc, two_addr)]

sample_server_args :: SA.Server_Options
sample_server_args =
  let cvm_ps :: CVM_SERV_Params
      cvm_ps = CVM_SERV_Params one_plc "SIG1"
      stype = CVM_SERV cvm_ps in
  SA.Server_Options False True one_addr stype

sample_client_args :: SA.Server_Options
sample_client_args =
  let cvm_ps :: CVM_SERV_Params
      cvm_ps = CVM_SERV_Params zero_plc "SIG0"
      stype = CVM_SERV cvm_ps in
  SA.Server_Options False True zero_addr stype


sample_aspmap :: M.Map ASP_ID String
sample_aspmap = M.fromList [(1, s)]
  where s = "" -- TODO: put asp socket here, TODO: don't hardcode...













{- This is a hard-coded initial state for demo/testing purposes.
   TODO:  Need to make this user-facing, provide an external registration process for ASP components.  -}

{-
am_state_init :: AM_St
am_state_init = empty_AM_state {- TODO: is this ok? -}
-}


{-
am_env_init :: AM_Env
am_env_init = 
  let -- Maps place to the asp_id that checks signatures from that place.
      app_sig_map = M.fromList [(0,41), (1,41)]
      -- Similar map for hash appraisals,
      app_hsh_map = M.empty
      -- Maps a place/asp_id pair to the asp_id that can appraise it.
      app_asp_map = M.fromList [((0, 1),42), ((1,1),42)]
      -- asp_id for (appraisal) asp that checks nonces
      app_nonceCheckAsp = 0 in
        initial_AM_env app_sig_map app_hsh_map app_asp_map app_nonceCheckAsp



vm_state_init = undefined
-}



{-
{- This is a hard-coded initial state for demo/testing purposes.
   TODO:  Need to make this user-facing, provide an external registration process for ASP, SIG, and Comm Server components.  -} 
vm_state_init :: EvidenceC  -> IO VM_St
vm_state_init e = do
  -- Register Comm Server
  commSocketPath <- lookupPath COMM
  -- Register my (place 0's) Signature server
  sigSocketPath <- lookupPath SIGN
  -- Register ASP 1
  asp1SocketPath <- lookupPath (ASP_SERV 1)
  -- Register appraiser of ASP 1
  app1SocketPath <- lookupPath (ASP_SERV 42)
  -- Register appraiser of 0's signature
  appSig0SocketPath <- lookupPath (ASP_SERV 41) 
  let aspMap =
        M.fromList
        [(1,asp1SocketPath),(42,app1SocketPath),(41,appSig0SocketPath)]
  return $ initial_VM_state e commSocketPath sigSocketPath aspMap
-}
