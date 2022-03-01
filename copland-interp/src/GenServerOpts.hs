module GenServerOpts where

import Copland

import Data.List(union)
import qualified Data.Map as M

cvm_server_addr :: Plc -> Address
cvm_server_addr p = "CVM_" ++ (show p)

par_server_addr :: Plc -> Address
par_server_addr p = "PAR_" ++ (show p)

sig_server_addr :: Plc -> Address
sig_server_addr p = "SIG_" ++ (show p)

store_server_addr :: Plc -> Address
store_server_addr p = "STORE_" ++ (show p)

asp_server_addr :: Plc -> ASP_ID -> Address
asp_server_addr p id = "ASP_" ++ (show p) ++ "_" ++ (show id)

get_server_addr_gen :: ServerType -> Plc -> Address
get_server_addr_gen st p =
  case st of
    SIGN -> sig_server_addr p
    STORE -> store_server_addr p
    ASP_SERV id -> asp_server_addr p id
    CVM_SERV _ -> cvm_server_addr p
    PAR_SERV _ -> par_server_addr p


get_places :: Term -> [Plc]
get_places t = getPlaces' t []

getPlaces' :: Term -> [Plc] -> [Plc]
getPlaces' t pls =
  case t of
   Coq_att p t' -> union [p] (getPlaces' t' pls)
   Coq_lseq t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bseq _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bpar _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   _ -> pls

get_asps' :: Term -> Plc -> [(Plc,ASP_ID)] -> [(Plc,ASP_ID)]
get_asps' t p as =
  case t of
   Coq_asp (ASPC (Coq_asp_paramsC i _ _ _)) -> union as [(p,i)]
   Coq_att q t' -> union as (get_asps' t' q as)
   Coq_lseq t1 t2 ->
     let ls = get_asps' t1 p as in
     let rs = get_asps' t2 p as in
     union ls rs
   Coq_bseq _ t1 t2 ->
     let ls = get_asps' t1 p as in
     let rs = get_asps' t2 p as in
     union ls rs
   Coq_bpar _ t1 t2 ->
     let ls = get_asps' t1 p as in
     let rs = get_asps' t2 p as in
     union ls rs
   _ -> as

get_asps :: Term -> Plc -> [(Plc,ASP_ID)]
get_asps t p = get_asps' t p []

get_asps_pair :: (Term,Plc) -> [(Plc,ASP_ID)]
get_asps_pair (t,p) = get_asps t p

get_asps_terms :: [Term] -> [Plc] -> [(Plc,ASP_ID)]
get_asps_terms ts ps =
  let prs = zip ts ps in
    concatMap get_asps_pair prs

update_one_aspmap :: M.Map Plc (M.Map ASP_ID String) -> (Plc, ASP_ID) -> M.Map Plc (M.Map ASP_ID String)
update_one_aspmap m (p,id) =
  let inner_map = 
        case (M.lookup p m) of
          Just im -> im
          Nothing -> M.empty
      new_innermap = M.insert id (asp_server_addr p id) inner_map in
    M.insert p new_innermap m
    
          
      

  --M.insert p id (asp_server_addr p id) inmap

get_sample_aspmap'' :: [(Plc,ASP_ID)] ->  M.Map Plc (M.Map ASP_ID String)
get_sample_aspmap'' ps = foldl update_one_aspmap M.empty ps

get_sample_aspmap' :: Term -> Plc -> M.Map Plc (M.Map ASP_ID String)
get_sample_aspmap' t p =
  get_sample_aspmap'' (get_asps t p)

get_sample_aspmap :: Term -> Plc -> M.Map ASP_ID String
get_sample_aspmap t p =
  let m = get_sample_aspmap' t p in
    m M.! p

one_name_map_pair :: Plc -> (Plc, Address)
one_name_map_pair p = (p, cvm_server_addr p)

gen_client_name_map :: [Plc] -> M.Map Plc Address
gen_client_name_map ps = M.fromList (map one_name_map_pair ps)

gen_name_map_term :: Term -> M.Map Plc Address
gen_name_map_term t =
  let ps = get_places t in
    gen_client_name_map ps
