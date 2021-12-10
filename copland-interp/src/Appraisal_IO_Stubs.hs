module Appraisal_IO_Stubs where

import Copland
import BS (BS, empty_bs, zero_bs, one_bs, bool_to_bs)
import MonadAM --GenOptMonad
import CryptoImpl (get_pubkey_simpl, verify_simplD)
import IO_Stubs(encodeEvRaw)

import Control.Monad.IO.Class (liftIO)

checkASP :: ASP_PARAMS -> BS -> AM BS
checkASP params bs = return one_bs

checkSigBits :: RawEv -> Plc -> BS -> AM BS
checkSigBits e p sigbits = do
  pubkey <- liftIO $ get_pubkey_simpl p
  let msgbits = encodeEvRaw e
  let res = verify_simplD pubkey msgbits sigbits
  return res

checkNonce :: Prelude.Int -> BS -> AM BS
checkNonce nid bs = do --return empty_bs
  goldenVal <- am_getNonce nid
  let bool_res = (bs == goldenVal)
  return (bool_to_bs bool_res)
