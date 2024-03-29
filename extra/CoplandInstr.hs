
module CoplandInstr where

import CoplandLang

data Instr
     = Copy
     | Umeas ASP_ID [ARG]
     | Sign
     | Hash
     | Split SP SP
     | Joins
     | Joinp
     | Reqrpy Pl T
     | Besr
     | Bep [Instr] [Instr]
  deriving (Read,Show)

instr_compiler :: T -> [Instr]

instr_compiler t =
  case t of
    CPY -> [Copy]
    ASP i args -> [Umeas i args]
    SIG -> [Sign]
    HSH -> [Hash]
    LN t1 t2 -> (instr_compiler t1) ++ (instr_compiler t2)
    BRS (sp1,sp2) t1 t2 -> [Split sp1 sp2] ++  (instr_compiler t1) ++ [Besr] ++  (instr_compiler t2) ++ [Joins]
    BRP (sp1,sp2) t1 t2 -> [Split sp1 sp2] ++  (instr_compiler t1) ++ [Besr] ++  (instr_compiler t2) ++ [Joins] -- TODO: for now (April 2020) compile to sequential
      --[Split sp1 sp2] ++  [Bep (instr_compiler t1)  (instr_compiler t2)] ++ [Joinp]
    AT p t1 -> [Reqrpy p t1]
