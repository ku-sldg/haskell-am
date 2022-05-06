{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

module Appraisal_IO_Stubs where

import Copland
import BS (BS, empty_bs, zero_bs, one_bs, bool_to_bs, bs_to_bool)
import MonadAM --GenOptMonad
import MonadAM_Types (AM, AM_St(..), empty_AM_env, runAM)  --import MonadAM_Types (AM_St(..), empty_AM_env, runAM)
import CryptoImpl (get_pubkey_simpl, verify_simplD)
import IO_Stubs(encodeEvRaw)
import ConcreteEvidence
import Control.Monad.IO.Class (liftIO)
import qualified Example_Phrases_Admits as EPA (cert_id, attest_id)
--import Impl_appraisal_alt(build_app_comp_evC)
--import ServerAppHandlers (appraise_attest_result)
import CommUtil (gen_run_client)

import qualified Data.Binary as BIN
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

checkASP :: ASP_PARAMS -> BS -> AM BS
checkASP params bs =
  case params of
    Coq_asp_paramsC i _ _ _ ->
      if | (i == EPA.cert_id) -> do

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

         | (i == EPA.attest_id) -> do
             let lazy_bs = BL.fromStrict bs
                 err_str = typed_error_str "AttestResult"
                 (r@(AttestResult t res_rawev)::AttestResult) = decodeBin err_str lazy_bs
                 nval = last res_rawev
             liftIO $ putStrLn $ "Nonce GRABBEDD: " ++ (show nval)
             --putStrLn $ "AttestResult grabbed: " ++ (show r)



             let reqParams = Coq_asp_paramsC 0 [] 0 0
                 e = [bs] ++ [nval]
                 reqm = (AspRequestMessage reqParams e)
                 addr = "APPRAISE" -- TODO: fix hardcode?
                 err_str = typed_error_str "AspResponseMessage"
             (rm :: AspResponseMessage) <- liftIO $ gen_run_client err_str addr reqm

             let lazy_bs = BL.fromStrict (aspBits rm)
                 err_str = typed_error_str "EvidenceC"
                 (ec::EvidenceC) = decodeBin err_str lazy_bs
                 app_res_bool = certWalk_EvidenceC ec




             
             liftIO $ putStrLn $ "returning from checkASP for 'attest' ASP"
             return (bool_to_bs app_res_bool) --(aspBits rm)

             --return BS.one_bs



             

             --appraise_attest_result ([bs] ++ [nval])


{-


             
             let them = 0 -- TODO: no hardcode?
                 init_ev_type = (Coq_nn 0) -- TODO: ok?
                 (et_app::Evidence) = eval t them init_ev_type
             --putStrLn $ "HERE" ++ (show et_app)
             liftIO $ putStrLn $ "Evidence Type computed for appraise ASP: " ++ (show et_app)
             let appraise_comp = build_app_comp_evC et_app res_rawev
                 amst = AM_St (M.fromList [(0,nval{-BS.empty_bs-})]) 1 -- TODO: need actual nonce value from attester/relying?

             ((app_res, _)::(EvidenceC, AM_St)) <- liftIO $ runAM appraise_comp empty_AM_env amst

             liftIO $ putStrLn $ "Appraisal EvidenceC structure computed: " ++ (show app_res)

             let resp_bs = BL.toStrict $ BIN.encode app_res
             return resp_bs
-}
             


             
         | otherwise -> return one_bs --error "Appraising unknown ASP ID" --return one_bs

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
