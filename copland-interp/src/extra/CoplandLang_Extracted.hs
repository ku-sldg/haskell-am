
{-
<   > 

Warning: The following axioms must be realized in the extracted
code: Term_Defs.ASP_ID Term_Defs.Arg Term_Defs.TARG_ID.
 [extraction-axiom-to-realize,extraction]

module Main where
-}

{- From:  Recursive Extraction Term_Defs.AnnoTermPar. -}
module CoplandLang_Extracted where

import qualified Prelude
import qualified Data.ByteString as B (ByteString, empty)

type Plc = Prelude.Int

{-
type ASP_ID = () -- AXIOM TO BE REALIZED
  

type TARG_ID = () -- AXIOM TO BE REALIZED
  

type Arg = () -- AXIOM TO BE REALIZED
-}

type N_ID = Prelude.Int

type BS = B.ByteString

type ASP_ID = Prelude.Int
type TARG_ID = Prelude.Int
type Arg = Prelude.String
  

data ASP_PARAMS =
   Asp_paramsC ASP_ID (([]) Arg) Plc TARG_ID
                deriving (Prelude.Read,Prelude.Show)

data ASP =
   CPY
 | ASPC ASP_PARAMS
 | SIG
 | HSH
  deriving (Prelude.Read,Prelude.Show)

data SP =
   ALL
 | NONE
  deriving (Prelude.Read,Prelude.Show)

type Split = (,) SP SP

data Term =
   Asp ASP
 | Att Plc Term
 | Lseq Term Term
 | Bseq Split Term Term
 | Bpar Split Term Term
  deriving (Prelude.Read,Prelude.Show)

type Loc = Prelude.Int

data AnnoTermPar =
   Aasp_par ASP
 | Aatt_par Plc Term
 | Alseq_par AnnoTermPar AnnoTermPar
 | Abseq_par Split AnnoTermPar AnnoTermPar
 | Abpar_par Loc Split AnnoTermPar Term
  deriving (Prelude.Read,Prelude.Show)



{- From:  'Recursive Extraction Term_Defs.EvC.' -}
data Evidence =
   Mt
 | Uu ASP_PARAMS Plc Evidence
 | Gg Plc Evidence
 | Hh Plc Evidence
 {-| Nn N_ID -} | Nn Prelude.Int
 | Ss Evidence Evidence
 | Pp Evidence Evidence
  deriving (Prelude.Read,Prelude.Show)

type RawEv = ([]) BS

data EvC =
   Evc RawEv Evidence
         deriving (Prelude.Read,Prelude.Show)

mt_evc :: EvC
mt_evc =
  Evc ([]) Mt

get_et :: EvC -> Evidence
get_et e =
  case e of {
   Evc _ et -> et}

get_bits :: EvC -> ([]) BS
get_bits e =
  case e of {
   Evc ls _ -> ls}
