{-  Json data exchange format for terms and messages involved with attestation protocol execution.  In particular, we use the Data.Aeson library to:

1)  encode Haskell datatypes (representing protocol terms and concrete evidence) to raw JSON.
2)  decode raw JSON to:
    a) Haskell datatypes representing the corresponding protocol terms and concrete evidence.
    b) Haskell datatypes representing the corresponding generic JSON object ASTs.

  Author: Adam Petz
  Date:  11/06/2018
-}

{-# LANGUAGE OverloadedStrings #-}
module JsonCopland where

--import Term_Defs
import CommTypes
--import CoplandLang
import Term_Defs
import BS
import ConcreteEvidence

import Data.Aeson
import Control.Exception
import System.IO.Error hiding (catch)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Base16 as B16 (encode,decode)
import qualified Data.Text.Encoding as DTE (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as TIO (putStrLn,appendFile)
import qualified Data.Text.Lazy.Encoding as T
import qualified System.Directory as SD (removeFile)

{- Confirm the input is in valid form, and return the value -}
decodeGen :: FromJSON a => BS -> IO a
decodeGen msg = do
          let val = decodeStrict msg
          case val of
            Nothing -> error $ "weird message received: " ++ (show msg)
            Just res -> return res
            
jsonToFile :: ToJSON a => a -> FilePath -> IO ()
jsonToFile x fp = (TIO.appendFile fp) . T.decodeUtf8 $ (encode x)

showJSON :: ToJSON a => a -> IO ()
showJSON x = TIO.putStrLn . T.decodeUtf8 $ (encode x)


{----------- HELPER/SUB-PARSER FUNCTIONS -----------}

{- Translates splitting function ADT to its integer representation -}
spToInt :: SP -> Int
spToInt sp =
  case sp of
  ALL -> 1
  NONE -> 0

{- Translates splitting function integer representation to corresponding ADT -}
intToSp :: Int -> SP
intToSp i =
  case i of
  0 -> NONE
  1 -> ALL
  _ -> error $ "splitting function not supported: " ++ (show i)

decodeSP :: Int -> Int -> (SP,SP)
decodeSP l r = ((intToSp l),(intToSp r))

{- Adapted from example here:  https://stackoverflow.com/questions/37054889/sending-receiving-binary-data-in-aeson -}
instance ToJSON B.ByteString where
  toJSON = String . DTE.decodeUtf8 . B16.encode

instance FromJSON B.ByteString where
  parseJSON (String t) = do
    let (val, extra) = B16.decode (DTE.encodeUtf8 t)
    case extra of
     "" -> return val
     _  -> error $ "error in FromJSON ByteString."
  parseJSON _ = error "error in FromJSON ByteString"

instance ToJSON SP where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }

instance FromJSON SP where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }

instance ToJSON Evidence where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }

instance FromJSON Evidence where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }


instance ToJSON EvidenceC where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }

instance FromJSON EvidenceC where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    {-, allNullaryToStringTag = False -}
    }

instance ToJSON ASP_PARAMS where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance FromJSON ASP_PARAMS where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance ToJSON ASP where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance FromJSON ASP where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance ToJSON Term where
  toJSON = genericToJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance FromJSON Term where
  parseJSON = genericParseJSON defaultOptions {
    sumEncoding =
       defaultTaggedObject {
         tagFieldName = "constructor",
         contentsFieldName = "data"
         }
    }

instance ToJSON RequestMessage
instance FromJSON RequestMessage

instance ToJSON ResponseMessage
instance FromJSON ResponseMessage

instance ToJSON SigRequestMessage
instance FromJSON SigRequestMessage

instance ToJSON SigResponseMessage
instance FromJSON SigResponseMessage

instance ToJSON AspRequestMessage
instance FromJSON AspRequestMessage

instance ToJSON AspResponseMessage
instance FromJSON AspResponseMessage

instance ToJSON StoreGetMessage
instance FromJSON StoreGetMessage

instance ToJSON StoreSetMessage
instance FromJSON StoreSetMessage

instance ToJSON StoreRequestMessage
instance FromJSON StoreRequestMessage

instance ToJSON StoreAckMessage
instance FromJSON StoreAckMessage

{-
instance ToJSON TestIncMessage
instance FromJSON TestIncMessage

instance ToJSON TestGetMessage
instance FromJSON TestGetMessage
-}

instance ToJSON TestRequestMessage
instance FromJSON TestRequestMessage

instance ToJSON TestResponseMessage
instance FromJSON TestResponseMessage

{-
instance ToJSON CommSetMessage
instance FromJSON CommSetMessage

instance ToJSON CommAckMessage
instance FromJSON CommAckMessage

instance ToJSON CommSetList
instance FromJSON CommSetList
-}

instance ToJSON StartMessagePar
instance FromJSON StartMessagePar

instance ToJSON WaitMessagePar
instance FromJSON WaitMessagePar

instance ToJSON RequestMessagePar
instance FromJSON RequestMessagePar

instance ToJSON ResponseMessagePar
instance FromJSON ResponseMessagePar



jsonOut :: Term -> RawEv -> RawEv {-EvidenceC -> EvidenceC-} -> IO ()
jsonOut t ev resEv = do
  let jsonProtoInFile = "../demoOutput/jsonIn.hs"
      jsonEvOutFile = "../demoOutput/jsonOut.hs"
      
  SD.removeFile jsonProtoInFile `catch` handleExists
  jsonToFile t jsonProtoInFile
  appendFile jsonProtoInFile "\n\n"
  jsonToFile ev jsonProtoInFile

  SD.removeFile jsonEvOutFile `catch` handleExists
  jsonToFile resEv jsonEvOutFile

  putStrLn ""
  putStrLn $ "File with JSON request(protocol+initial evidence): demoOutput/jsonIn.hs"
  putStrLn $ "File with JSON response(resulting evidence): demoOutput/jsonOut.hs"
  return ()

 where handleExists e
         | isDoesNotExistError e = return ()
         | otherwise = throwIO e
