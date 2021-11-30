{-# LANGUAGE DeriveGeneric #-}
module CommTypes where

import Term_Defs
import Term_Defs_Deriving
import BS

import System.Environment (lookupEnv)
import GHC.Generics (Generic)
import Control.Concurrent.STM
import Numeric.Natural
import qualified Data.Map as M (Map)



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
    reqEv :: RawEv } deriving (Show,Read,Generic)

--Attestation Response Message
data ResponseMessage = ResponseMessage
  { respToPlace :: Plc,
    respFromPlace :: Plc,
    respEv :: RawEv } deriving (Show,Read,Generic)

--Attestation Request Message
data SigRequestMessage = SigRequestMessage
  { evBits :: BS } deriving (Show,Read,Generic)

--Attestation Response Message
data SigResponseMessage = SigResponseMessage
  { sigBits :: BS } deriving (Show,Read,Generic)

--Attestation Request Message
data AspRequestMessage = AspRequestMessage
  { aspArgs :: [Arg] } deriving (Show,Read,Generic)

--Attestation Response Message
data AspResponseMessage = AspResponseMessage
  { aspBits :: BS } deriving (Show,Read,Generic)

data StoreSetMessage = StoreSetMessage
  { myId  :: VM_ID,
    init_ev_id :: Natural,
    inEv  :: RawEv } deriving (Show,Read,Generic)

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
  { outEv :: RawEv } deriving (Show,Read,Generic)

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
    init_cell :: TMVar RawEv,
    final_cell :: TMVar RawEv
    --reqEv :: Ev
  }
  deriving ({-Show,Read,-}Generic)

data CommParMessage =
  CommParMessage
  { parToPl :: Plc,
    parReqNameMap :: M.Map Plc Address,
    parTerm :: Term,
    par_init_cell :: TMVar RawEv,
    par_final_cell :: TMVar RawEv
  }
  deriving ({-Show,Read,-}Generic)

--Parallel Request Message
data RequestMessagePar = RequestMessagePar
  { toPlacePar :: Plc,
    --fromPlacePar :: Plc,
    reqNameMapPar :: M.Map Plc Address,
    reqTermPar :: Term,
    reqEvPar :: RawEv } deriving (Show,Read,Generic)

--Parallel Response Message
data ResponseMessagePar = ResponseMessagePar
  { --respToPlacePar :: Plc,
    --respFromPlacePar :: Plc,
    respEvPar :: RawEv } deriving (Show,Read,Generic)

{-
data CommReqList = CommReqList [CommReqMessage] deriving ({-Show,Read,-}Generic)

data CommAckMessage =
  CommAckMessage deriving (Show,Read,Generic)
-}






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
