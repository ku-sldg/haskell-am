-- {-# LANGUAGE StandaloneDeriving  #-}
module Copland_Concrete where

import Term_Defs

data CoplandTerm =
  CPY
  | ASP ASP_ID [Arg] Plc TARG_ID
  | SIG
  | HSH
  | AT Plc CoplandTerm
  | CoplandTerm :->: CoplandTerm
  | CoplandTerm :-<-: CoplandTerm
  | CoplandTerm :-<+: CoplandTerm
  | CoplandTerm :+<-: CoplandTerm
  | CoplandTerm :+<+: CoplandTerm

  | CoplandTerm :-~-: CoplandTerm
  | CoplandTerm :-~+: CoplandTerm
  | CoplandTerm :+~-: CoplandTerm
  | CoplandTerm :+~+: CoplandTerm  deriving (Show,Read)


toExtractedTerm :: CoplandTerm -> Term
toExtractedTerm ct =
  case ct of
    Copland_Concrete.CPY -> Coq_asp Term_Defs.CPY
    Copland_Concrete.ASP i args tpl tid -> Coq_asp (ASPC (Coq_asp_paramsC i args tpl tid))
    Copland_Concrete.SIG -> Coq_asp Term_Defs.SIG
    Copland_Concrete.HSH -> Coq_asp Term_Defs.HSH
    AT q t' -> Coq_att q (toExtractedTerm t')
    t1 :->: t2 -> Coq_lseq (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :-<-: t2 -> Coq_bseq (NONE,NONE) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :-<+: t2 -> Coq_bseq (NONE,ALL) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :+<-: t2 -> Coq_bseq (ALL,NONE) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :+<+: t2 -> Coq_bseq (ALL,ALL) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :-~-: t2 -> Coq_bpar (NONE,NONE) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :-~+: t2 -> Coq_bpar (NONE,ALL) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :+~-: t2 -> Coq_bpar (ALL,NONE) (toExtractedTerm t1) (toExtractedTerm t2)
    t1 :+~+: t2 -> Coq_bpar (ALL,ALL) (toExtractedTerm t1) (toExtractedTerm t2)
