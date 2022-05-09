{-  Library of initial states for one-off Demos.
    TODO:  Make these more user-facing and externally configurable.

  Author: Adam Petz
  Date:  04/22/2020

-}

{-# LANGUAGE ScopedTypeVariables #-}

module DemoStates where

import Copland
--import MonadAM
--import MonadVM_Old
--import qualified ServerProgArgs as SA (Server_Options(..))
import ServerOpts(cvm_server_addr, sig_server_addr, par_server_addr, get_places, gen_name_map_term, get_sample_aspmap, get_asps)
import MonadCop(Cop_Env(..))
import qualified Example_Phrases_Admits as EPA (cache_id, attest_id, appraise_id, cert_id)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

default_asp_addr :: Address
default_asp_addr = "ASP_DEFAULT"

local_namemap :: M.Map Plc Address
local_namemap =  M.fromList [(zero_plc, zero_addr),
                           (one_plc, one_addr),
                           (two_plc, two_addr),
                           (three_plc, three_addr),
                           (four_plc, four_addr)
                          ]

local_aspmap :: M.Map ASP_ID Address
local_aspmap = M.fromList [(EPA.cache_id, "CACHE"),
                           (EPA.attest_id, "ATTEST"),
                           (EPA.appraise_id, "APPRAISE"),
                           (EPA.cert_id, "CERT")
                          ]
  
update_one_aspmap :: M.Map ASP_ID Address -> ASP_ID -> M.Map ASP_ID Address
update_one_aspmap m id =
  let addr = fromMaybe default_asp_addr (M.lookup id local_aspmap) in
    M.insert id addr m

build_local_aspmap :: Term -> Plc -> M.Map ASP_ID Address
build_local_aspmap t p =
  --let amap = get_sample_aspmap t p
      --asp_ids = M.keys amap in
    let asp_ids = map snd (get_asps t p) in
      foldl update_one_aspmap M.empty asp_ids

sample_cop_env :: Bool -> Bool -> Bool -> Bool -> Term -> Plc -> Cop_Env
sample_cop_env simB debugB cvmSpawn_b aspSpawn_b t p =
  let nm =
        case cvmSpawn_b of
          True -> gen_name_map_term t
          False -> local_namemap
          
      store = undefined
      
      myAsps =
        case aspSpawn_b of
          True -> get_sample_aspmap t p
          False -> build_local_aspmap t p
          
      sm = Sign_Server_Addr (sig_server_addr p)
      par_addr = par_server_addr p in
      
    Cop_Env simB debugB nm sm p store myAsps par_addr
      



zero_plc :: Plc
zero_plc = 0

zero_addr :: Address
zero_addr = "CVM_0"

one_plc :: Plc
one_plc = 1

one_addr :: Address
one_addr = "CVM_1"

two_plc :: Plc
two_plc = 2

two_addr :: Address
two_addr = "CVM_2"

three_plc :: Plc
three_plc = 3

three_addr :: Address
three_addr = "CVM_3"

four_plc :: Plc
four_plc = 4

four_addr :: Address
four_addr = "CVM_4"

{-

two_addr :: Address
two_addr = "CVM_2"

sig_zero_addr :: Address
sig_zero_addr = "SIG_0"

sig_one_addr :: Address
sig_one_addr = "SIG_1"

asp_one_addr :: Address
asp_one_addr = "ASP_2_1"



sample_client_name_map :: M.Map Plc Address
sample_client_name_map = M.fromList [(zero_plc, zero_addr), (one_plc, one_addr), (two_plc, two_addr)]

sample_server_args :: SA.Server_Options
sample_server_args =
  let cvm_ps :: CVM_SERV_Params
      cvm_ps = CVM_SERV_Params one_plc (Sign_Server_Addr sig_one_addr)
      stype = CVM_SERV cvm_ps in
  SA.Server_Options False True one_addr stype

sample_client_args :: SA.Server_Options
sample_client_args =
  let cvm_ps :: CVM_SERV_Params
      cvm_ps = CVM_SERV_Params zero_plc (Sign_Server_Addr sig_zero_addr)
      stype = CVM_SERV cvm_ps in
  SA.Server_Options False True zero_addr stype


sample_aspmap :: M.Map ASP_ID String
sample_aspmap = M.fromList [(1, s)]
  where s = asp_one_addr -- TODO: don't hardcode...
-}













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
