-- {-# LANGUAGE StandaloneDeriving  #-}
module Example_Phrases_Concrete where

import Copland_Concrete
import Term_Defs hiding (ASP(..), Term(..))
import Example_Phrases_Admits

p0 :: Plc
p0 = 0

p1 :: Plc
p1 = 1

p2 :: Plc
p2 = 2

p3 :: Plc
p3 = 3

p4 :: Plc
p4 = 4

fileHash_asp :: FilePath -> CoplandTerm
fileHash_asp fp =
  (ASP fh_id [fp] p targ_id) :->: SIG
  
  where fh_id :: ASP_ID
        fh_id = 5

        p :: Plc
        p = p1

        targ_id :: TARG_ID
        targ_id = 0  -- targ_id is arbitrary for now.
          -- Could identify a particular file system at Place p1.

fileHash_asp_request :: FilePath -> CoplandTerm
fileHash_asp_request fp =
  AT p (fileHash_asp fp)
  
  where p :: Plc
        p = p1

simple_asp_phrase :: CoplandTerm
simple_asp_phrase = ASP 1 ["arg1", "arg2"] p2 3

simple_at_phrase :: CoplandTerm
simple_at_phrase = AT 1 simple_asp_phrase

simple_ln_phrase :: CoplandTerm
simple_ln_phrase = SIG :->: SIG

simple_bseq_phrase :: CoplandTerm
simple_bseq_phrase = SIG :+<-: SIG

simple_bpar_phrase :: CoplandTerm
simple_bpar_phrase = SIG :+~-: SIG


cpy_phrase :: CoplandTerm
cpy_phrase = CPY

sig_phrase :: CoplandTerm
sig_phrase = SIG

hsh_phrase :: CoplandTerm
hsh_phrase = HSH

attest :: Plc -> TARG_ID -> CoplandTerm
attest p tid = ASP attest_id [] p tid

appraise :: Plc -> TARG_ID -> CoplandTerm
appraise p tid = ASP appraise_id [] p tid

appraise_bgweak :: Plc -> TARG_ID -> CoplandTerm
appraise_bgweak p tid = ASP appraise_id app_bg_weak_args p tid

appraise_bgstrong :: Plc -> TARG_ID -> CoplandTerm
appraise_bgstrong p tid = ASP appraise_id app_bg_strong_args p tid

certificate :: Plc -> TARG_ID -> CoplandTerm
certificate p tid = ASP cert_id [] p tid

store :: Plc -> TARG_ID -> CoplandTerm
store p tid = ASP cache_id store_args p tid

retrieve :: Plc -> TARG_ID -> CoplandTerm
retrieve p tid = ASP cache_id retrieve_args p tid

cert_style_gen :: CoplandTerm -> CoplandTerm
cert_style_gen certTerm =
    AT p1 (
     (attest p1 sys) :->:
     (AT p2 (
         (appraise p2 sys) :->: certTerm)))

{-
pg 29:16 top, Certificate-Style section 
 -}
cert_style_simple_sig :: CoplandTerm
cert_style_simple_sig = cert_style_gen SIG


{-
pg 29:15, Certificate-Style section 
-}  
cert_style :: CoplandTerm
cert_style = cert_style_gen (certificate p2 sys)


{-
pg. 29:16 bottom, Certificate-Style section
-}
cert_cache_p1 :: CoplandTerm
cert_cache_p1 =
  (attest p1 sys) :->:
  (AT p2 (
      (appraise p2 sys) :->: (certificate p2 sys))) :->:
  (store p1 cache)


cert_cache_p0 :: CoplandTerm
cert_cache_p0 =
  AT p1 (
     ((retrieve p1 cache) :-<+: CPY) :->:
     SIG)


bg_check_gen :: Plc -> Plc -> CoplandTerm
bg_check_gen p q =
  (AT p (attest p sys)) :->: (AT q (appraise q sys))

{-
pg. 29:17, Background Check section
-}
bg_check :: CoplandTerm
bg_check = bg_check_gen p1 p2

{-
pg. 29:18, Parallel Mutual Attestation section
-}
par_mut_p0 :: CoplandTerm
par_mut_p0 = bg_check_gen p1 p2

par_mut_p1 :: CoplandTerm
par_mut_p1 = bg_check_gen p0 p2
  
layered_bg' :: CoplandTerm
layered_bg' = (attest p1 sys) :->:
              (attest p3 att_tid) :->:
              (attest p4 att_tid)

layered_bg'' :: CoplandTerm
layered_bg'' =
  (AT p3 (attest p3 sys)) :+~+: (AT p4 (attest p4 sys))  -- :+<+: :+~+:


layered_bg_weak_prefix :: CoplandTerm
layered_bg_weak_prefix = 
  AT p1 (
     (layered_bg' :+~+: layered_bg''))

layered_bg_strong_prefix :: CoplandTerm
layered_bg_strong_prefix = 
  AT p1 (
     (layered_bg' :+<+: layered_bg''))

{-
pg. 29:19, Layered Background Check section
-}
layered_bg_weak :: CoplandTerm
layered_bg_weak =
  AT p1 (
     (layered_bg' :+~+: layered_bg'') :->: -- :+<+: :+~+:
     (AT p2 (
         (appraise_bgweak p2 it) :->: SIG)))

{-
pg. 29:20, Layered Background Check section 
-}
layered_bg_strong :: CoplandTerm
layered_bg_strong =
    AT p1 (
     (layered_bg' :+<+: layered_bg'') :->:
     (AT p2 (
         (appraise_bgstrong p2 it) :->: SIG)))
         
