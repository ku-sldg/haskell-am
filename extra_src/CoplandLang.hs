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
import System.Environment (lookupEnv)

import Control.Concurrent.STM
import Numeric.Natural

{-  Identify Places (protocol participants)  -}
type Plc = Int
{-  Arguments to measurement commands (USM/KIM)  -}
type ARG = String

type Loc = Int

type ASP_ID = Int
type TARG_ID = Int
type N_ID = Int

{-  Evidence splitting functions.
    ALL-  keep all evidence
    NONE- keep no evidence -}
data SP = ALL | NONE
        deriving (Generic,Read,Show)

instance BI.Binary SP where

data ASP_PARAMS
  = ASP_PARAMSC ASP_ID [ARG] Plc TARG_ID
  deriving (Generic,Eq,Read,Show)

instance BI.Binary ASP_PARAMS where

data ASP
  = CPY
  | SIG
  | HSH
  | ASPC ASP_PARAMS
  deriving (Generic,Read,Show)

instance BI.Binary ASP where
  
-- Attestation Protocol Descrption Term.
data Term
  = ASPT ASP
  | AT Plc Term
  | LN Term Term
  | BRS (SP,SP) Term Term
  | BRP (SP,SP) Term Term  
  deriving (Generic,Read,Show)

{- TODO: shorthand notation for ASP terms, avoid ASPT x -}

{-
type Range = (Natural,Natural)

data AnnoTerm
  = AASPT Range ASP
  | AAT Range Pl AnnoTerm
  | ALN Range AnnoTerm AnnoTerm
  | ABRS Range (SP,SP) AnnoTerm AnnoTerm
  | ABRP Range (SP,SP) AnnoTerm AnnoTerm
  deriving (Generic,Read,Show)

{-
data CommSetMessage = CommSetMessage
  { toPl :: Pl,
    --fromPlace :: Pl,
    toNameMap :: M.Map Pl Address,
    toTerm :: T,
    init_cell :: TMVar Ev,
    final_cell :: TMVar Ev
    --reqEv :: Ev
  } deriving (Show,Read,Generic)

data CommSetList = CommSetList [CommSetMessage] deriving (Show,Read,Generic)

data CommAckMessage =
  CommAckMessage deriving (Show,Read,Generic)
-}

instance BI.Binary T where

range :: AnnoTerm -> Range
range t =
  case t of
    AASPT r _ -> r
    AAT r _ _ -> r
    ALN r _ _ -> r
    ABRS r _ _ _ -> r
    ABRP r _ _ _ -> r

anno :: T -> Natural -> (Natural, AnnoTerm)
anno t i =
  case t of
    ASPT a -> (i + 1, AASPT (i, i + 1) a)
    AT p x ->
      let (j,a) = anno x (i+1) in
        (j + 1, AAT (i, j + 1) p a)
    LN x y ->
      let (j,a) = anno x i in
      let (k,b) = anno y j in
      (k, ALN (i,k) a b)
    BRS s x y ->
      let (j,a) = anno x (i+1) in
      let (k,b) = anno y j in
      (k + 1, ABRS (i, k + 1) s a b)
    BRP s x y ->
      let (j,a) = anno x (i+1) in
      let (k,b) = anno y j in
      (k + 1, ABRP (i, k + 1) s a b)

annotated :: T -> AnnoTerm
annotated t = snd (anno t 0)

unanno :: AnnoTerm -> T
unanno a =
  case a of
    AASPT _ a' -> ASPT a'
    AAT _ q t -> AT q (unanno t)
    ALN _ t1 t2 -> LN (unanno t1) (unanno t2)
    ABRS _ sp t1 t2 -> BRS sp (unanno t1) (unanno t2)
    ABRP _ sp t1 t2 -> BRP sp (unanno t1) (unanno t2)
   -}


{-  Raw bits (results of measurement, hashes, nonces)  -}
type BS = B.ByteString

type RawEv = ([]) BS

data EvC
  = EvC RawEv Evidence


-- Concrete Evidence returned from an execution.
data EvidenceC
  = Mt
  | N N_ID BS EvidenceC
  | U ASP_PARAMS Plc BS EvidenceC
  | G Plc BS EvidenceC
  | H Plc BS Evidence
  | SS EvidenceC EvidenceC
  | PP EvidenceC EvidenceC
  deriving (Generic,Eq, Read, Show)

instance BI.Binary EvidenceC  where
--instance NFData Ev where


-- Place-lifted Evidence, used as intermediate for generating appraisal term
data Evidence
  = Mtt
  | Ut Plc ASP_PARAMS Evidence
  | Gt Plc Evidence
  | Ht Plc Evidence
  | Nt N_ID Evidence
  | SSt Evidence Evidence
  | PPt Evidence Evidence
  deriving (Generic,Eq,Read, Show)

instance BI.Binary Evidence where


{-------- Comm Types --------}
--Abstract Address type for concrete Addresses (i.e. IP:port string).
type Address = String

type VM_ID = Natural

--Attestation Request Message
data RequestMessage = RequestMessage
  { toPlace :: Plc,
    fromPlace :: Plc,
    reqNameMap :: M.Map Plc Address,
    reqTerm :: Term,
    reqEv :: EvidenceC } deriving (Show,Read,Generic)

--Attestation Response Message
data ResponseMessage = ResponseMessage
  { respToPlace :: Plc,
    respFromPlace :: Plc,
    respEv :: EvidenceC } deriving (Show,Read,Generic)

--Attestation Request Message
data SigRequestMessage = SigRequestMessage
  { evBits :: BS } deriving (Show,Read,Generic)

--Attestation Response Message
data SigResponseMessage = SigResponseMessage
  { sigBits :: BS } deriving (Show,Read,Generic)

--Attestation Request Message
data AspRequestMessage = AspRequestMessage
  { aspArgs :: [ARG] } deriving (Show,Read,Generic)

--Attestation Response Message
data AspResponseMessage = AspResponseMessage
  { aspBits :: BS } deriving (Show,Read,Generic)

data StoreSetMessage = StoreSetMessage
  { myId  :: VM_ID,
    init_ev_id :: Natural,
    inEv  :: EvidenceC } deriving (Show,Read,Generic)

data StoreGetMessage = StoreGetMessage
  { get_id :: Natural } deriving (Show,Read,Generic)

--Store Request (Set/Get) Message
data StoreRequestMessage =
  SetMessage StoreSetMessage
  | GetMessage StoreGetMessage deriving (Show,Read,Generic)

--Store Acknowledge Message
data StoreAckMessage =
  StoreAckMessage
  { real_ev_id :: Natural } deriving (Show,Read,Generic)

--Store Response Message
data StoreResponseMessage =
  StoreResponseMessage
  { outEv :: EvidenceC } deriving (Show,Read,Generic)

{-
--Test Inc Message
data TestIncMessage =
  TestIncMessage
   deriving (Show,Read,Generic)

--Test Get Message
data TestGetMessage =
  TestGetMessage
  deriving (Show,Read,Generic)
-}

--Test Request (Inc/Get) Message
data TestRequestMessage =
  TestIncMessage
  | TestGetMessage  deriving (Show,Read,Generic)

--Test Response Message
data TestResponseMessage =
  TestResponseMessage
  { outN :: Natural } deriving (Show,Read,Generic)

data CommReqMessage =
  ReqMessage CommSetMessage
  | ParMessage CommParMessage
  deriving ({-Show,Read,-}Generic)

data CommSetMessage =
  CommSetMessage
  { toPl :: Plc,
    fromPl :: Plc,
    toNameMap :: M.Map Plc Address,
    toTerm :: Term,
    init_cell :: TMVar EvidenceC,
    final_cell :: TMVar EvidenceC
    --reqEv :: Ev
  }
  deriving ({-Show,Read,-}Generic)

data CommParMessage =
  CommParMessage
  { parToPl :: Plc,
    parReqNameMap :: M.Map Plc Address,
    parTerm :: Term,
    par_init_cell :: TMVar EvidenceC,
    par_final_cell :: TMVar EvidenceC
  }
  deriving ({-Show,Read,-}Generic)

--Parallel Request Message
data RequestMessagePar = RequestMessagePar
  { toPlacePar :: Plc,
    --fromPlacePar :: Plc,
    reqNameMapPar :: M.Map Plc Address,
    reqTermPar :: Term,
    reqEvPar :: EvidenceC } deriving (Show,Read,Generic)

--Parallel Response Message
data ResponseMessagePar = ResponseMessagePar
  { --respToPlacePar :: Plc,
    --respFromPlacePar :: Plc,
    respEvPar :: EvidenceC } deriving (Show,Read,Generic)

{-
data CommReqList = CommReqList [CommReqMessage] deriving ({-Show,Read,-}Generic)

data CommAckMessage =
  CommAckMessage deriving (Show,Read,Generic)
-}
  
{- Cannonical way of taking concrete evidence to its bits
   (Prep for signing, hashing, etc.).  -}
encodeEv :: EvidenceC -> BS
encodeEv e =
  case e of
  Mt -> B.empty
  U _ _ bs e' ->
    let e1bs = (encodeEv e') in
        (B.append e1bs bs)
  G _ bs _ -> bs
  H _ bs _ -> bs
  N _ bs e' ->
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


data ServerType =
  COMM
  | SIGN
  | ASP_SERV ASP_ID
  | STORE
  | PAR

-- socketPathname is currently a global constant here
-- this must change, soon ....
lookupPath :: ServerType -> IO FilePath
lookupPath v = do
  let tag =
        case v of
        COMM -> "COMM"
        PAR -> "PAR"
        SIGN -> "SIG"
        STORE -> "STORE"
        ASP_SERV i -> "ASP_" ++ (show i)
  let custom_path = "COPLAND_" ++ tag ++ "_SOCKET"
  maybeBuildPath <- lookupEnv "COPLAND_BUILD" -- TODO: fix hardcoding
  maybeSocketPath  <- lookupEnv $ custom_path
  socketPath <-
        case maybeSocketPath of
        Just p -> return p
        Nothing ->
          case maybeBuildPath of
           Just s -> do
             return $ s ++ "/" ++ tag
           Nothing ->
             error $ "Missing both COPLAND_BUILD(for default path) and " ++ custom_path ++ "(for custom path) environment variables.  Must have one or the other to connect to the " ++ tag ++ "Server."
  return socketPath



