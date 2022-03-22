{-# LANGUAGE ScopedTypeVariables #-}

module Appraisal_IO_Stubs where

import Copland
import BS (BS, empty_bs, zero_bs, one_bs, bool_to_bs, bs_to_bool)
import MonadAM --GenOptMonad
import MonadAM_Types (AM)
import CryptoImpl (get_pubkey_simpl, verify_simplD)
import IO_Stubs(encodeEvRaw)
import ConcreteEvidence
import Control.Monad.IO.Class (liftIO)
import qualified Example_Phrases_Admits as EPA (cert_id)

import qualified Data.Binary as BIN
import qualified Data.ByteString.Lazy as BL

checkASP :: ASP_PARAMS -> BS -> AM BS
checkASP params bs =
  case params of
    Coq_asp_paramsC i _ _ _ ->
      case (i == EPA.cert_id) of
        True -> do

          {-
          let lazy_bs = BL.fromStrict bs
              (r::EvidenceC) = BIN.decode lazy_bs
          liftIO $ putStrLn $ "EvidenceC structure walked for Cert ASP: " ++ (show r)
          let b = certWalk_EvidenceC r
          liftIO $ putStrLn $ "Checking Cert ASP, bool result: " ++ show (b)
-}
          liftIO $ putStrLn $ "Appraising Cert ASP, bool result: " ++ show (BS.bs_to_bool bs)
          return bs -- TODO: is simple forwading of cert BS ok here?
          --return (BS.bool_to_bs b)
        False -> return one_bs

  --return one_bs

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
