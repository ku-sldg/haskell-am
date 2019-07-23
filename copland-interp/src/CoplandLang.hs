{-  Copland language definition.
    
   -Term(T):  protocol action.
   -Concrete evidence(Ev):  data result of protocol execution.

  Author: Adam Petz
  Date:  11/06/2018
-}

{-# LANGUAGE DeriveGeneric #-}

module CoplandLang where

import GHC.Generics (Generic)
import qualified Data.ByteString as B (ByteString, append, empty)
import qualified Data.Binary as BI (Binary)
import qualified Data.Map as M (Map)

{-  Identify Places (protocol participants)  -}
type Pl = Int
{-  Arguments to measurement commands (USM/KIM)  -}
type ARG = String
{-  Raw bits (results of measurement, hashes, nonces)  -}
type BS = B.ByteString

type ASP_ID = Int

{-  Evidence splitting functions.
    ALL-  keep all evidence
    NONE- keep no evidence -}
data SP = ALL | NONE
        deriving (Generic,Read,Show)

instance BI.Binary SP where
  
-- Attestation Protocol Descrption Term.
data T
  = USM ASP_ID [ARG]
  | KIM ASP_ID Pl [ARG]
  | SIG
  | HSH
  | CPY
  | AT Pl T
  | LN T T
  | BRS (SP,SP) T T
  | BRP (SP,SP) T T  
  deriving (Generic,Read,Show)

instance BI.Binary T where

-- Concrete Evidence returned from an execution.
data Ev
  = Mt
  | U ASP_ID [ARG] Pl BS Ev
  | K ASP_ID [ARG] Pl Pl BS Ev
  | G Pl Ev BS
  | H Pl BS
  | N Pl Int BS Ev
  | SS Ev Ev
  | PP Ev Ev
  deriving (Generic,Eq, Read, Show)

instance BI.Binary Ev where
--instance NFData Ev where


{-------- Comm Types --------}
--Abstract Address type for concrete Addresses (i.e. IP:port string).
type Address = String

--Attestation Request Message
data RequestMessage = RequestMessage
                      { toPlace :: Pl,
                        fromPlace :: Pl,
                        reqNameMap :: M.Map Pl Address,
                        reqTerm :: T,
                        reqEv :: Ev
                      } deriving (Show,Read,Generic)

--Attestation Response Message
data ResponseMessage = ResponseMessage
                      { respToPlace :: Pl,
                        respFromPlace :: Pl,
                        respEv :: Ev
                      } deriving (Show,Read,Generic)
  
{- Cannonical way of taking concrete evidence to its bits
   (Prep for signing, hashing, etc.).  -}
encodeEv :: Ev -> BS
encodeEv e =
  case e of
  Mt -> B.empty
  U _ _ _ bs e' ->
    let e1bs = (encodeEv e') in
        (B.append e1bs bs)
  K _ _ _ _ bs e' ->
    let e1bs = (encodeEv e') in
        (B.append e1bs bs)
  G _ _ bs -> bs
  H _ bs -> bs
  N _ _ bs e' ->
    let e1bs = (encodeEv e') in
        (B.append e1bs bs)
  SS e1 e2 ->
    let e1bs = (encodeEv e1) in
    let e2bs = (encodeEv e2) in
    (B.append e1bs e2bs)
  PP e1 e2 ->
    let e1bs = (encodeEv e1) in
    let e2bs = (encodeEv e2) in
    (B.append e1bs e2bs)


