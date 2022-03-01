
module Example_Phrases_Admits where

import Term_Defs

attest_id :: ASP_ID
attest_id = 1

appraise_id :: ASP_ID
appraise_id = 2

cert_id :: ASP_ID
cert_id = 3

{-
store_id :: ASP_ID
store_id = 4

retrieve_id :: ASP_ID
retrieve_id = 5
-}

cache_id :: ASP_ID
cache_id = 4

store_args :: [Arg]
store_args = ["store"]

retrieve_args :: [Arg]
retrieve_args = ["retrieve"]



sys :: TARG_ID
sys = 1

cache :: TARG_ID
cache = 2

att_tid :: TARG_ID
att_tid = 3

it :: TARG_ID
it = 4
