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
import Numeric.Natural

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

data ASP
  = CPY
  | SIG
  | HSH
  | ASPC ASP_ID [ARG]
  deriving (Generic,Read,Show)

instance BI.Binary ASP where
  
-- Attestation Protocol Descrption Term.
data T
  = ASPT ASP
  | AT Pl T
  | LN T T
  | BRS (SP,SP) T T
  | BRP (SP,SP) T T  
  deriving (Generic,Read,Show)

{- TODO: shorthand notation for ASP terms, avoid ASPT x -}

type Range = (Natural,Natural)

data AnnoTerm
  = AASPT Range ASP
  | AAT Range Pl AnnoTerm
  | ALN Range AnnoTerm AnnoTerm
  | ABRS Range (SP,SP) AnnoTerm AnnoTerm
  | ABRP Range (SP,SP) AnnoTerm AnnoTerm
  deriving (Generic,Read,Show)

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



-- Concrete Evidence returned from an execution.
data Ev
  = Mt
  | U ASP_ID [ARG] BS Ev
  | G BS Ev
  | H BS
  | N Int BS Ev
  | SS Ev Ev
  | PP Ev Ev
  deriving (Generic,Eq, Read, Show)

instance BI.Binary Ev where
--instance NFData Ev where


-- Place-lifted Evidence, used as intermediate for generating appraisal term
data Ev_T
  = Mtt
  | Ut Pl ASP_ID [ARG] Ev_T
  | Gt Pl Ev_T
  | Ht Pl
  | Nt Int Ev_T
  | SSt Ev_T Ev_T
  | PPt Ev_T Ev_T
  deriving (Generic,Eq, Read, Show)

instance BI.Binary Ev_T where


{-------- Comm Types --------}
--Abstract Address type for concrete Addresses (i.e. IP:port string).
type Address = String

type VM_ID = Natural

--Attestation Request Message
data RequestMessage = RequestMessage
  { toPlace :: Pl,
    fromPlace :: Pl,
    reqNameMap :: M.Map Pl Address,
    reqTerm :: T,
    reqEv :: Ev } deriving (Show,Read,Generic)

--Attestation Response Message
data ResponseMessage = ResponseMessage
  { respToPlace :: Pl,
    respFromPlace :: Pl,
    respEv :: Ev } deriving (Show,Read,Generic)

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
    setId :: Natural,
    inEv  :: Ev } deriving (Show,Read,Generic)

data StoreGetMessage = StoreGetMessage
  { getId :: Natural } deriving (Show,Read,Generic)

--Store Request (Set/Get) Message
data StoreRequestMessage =
  SetMessage StoreSetMessage
  | GetMessage StoreGetMessage deriving (Show,Read,Generic)

--Store Acknowledge Message
data StoreAckMessage =
  StoreAckMessage
  { realId :: Natural } deriving (Show,Read,Generic)

--Store Response Message
data StoreResponseMessage =
  StoreResponseMessage
  { outEv :: Ev } deriving (Show,Read,Generic)

  
{- Cannonical way of taking concrete evidence to its bits
   (Prep for signing, hashing, etc.).  -}
encodeEv :: Ev -> BS
encodeEv e =
  case e of
  Mt -> B.empty
  U _ _ bs e' ->
    let e1bs = (encodeEv e') in
        (B.append e1bs bs)
  G bs _ -> bs
  H bs -> bs
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

-- socketPathname is currently a global constant here
-- this must change, soon ....
lookupPath :: ServerType -> IO FilePath
lookupPath v = do
  let tag =
        case v of
        COMM -> "COMM"
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
