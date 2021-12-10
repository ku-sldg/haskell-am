module Appraisal_IO_Stubs where

import Copland
import BS (BS, empty_bs)
import GenOptMonad

checkASP :: ASP_PARAMS -> BS -> AM BS
checkASP params bs = return empty_bs

checkSigBits :: RawEv -> Plc -> BS -> AM BS
checkSigBits e p bs = return empty_bs

checkNonce :: Prelude.Int -> BS -> AM BS
checkNonce nid bs = return empty_bs
