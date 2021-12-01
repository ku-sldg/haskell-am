{-  Experimental communication primitives and infrastructure for evaluation of remote attestation protocols.

  Author: Adam Petz
  Date:  11/18/2018, 5/13/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module CommImpl where

import Copland

import System.IO.Error hiding (catch)
import Control.Exception (throwIO, catch)
import qualified Data.Aeson as DA (decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified JsonCopland as JC (jsonToFile)
import qualified System.Directory as SD (removeFile)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import qualified Data.Map as M(Map)

{-  Send an attestation request
    Params:
      pTo- intended recipient of request
      pFrom- requester place
      t-   term for remote entity to execute
      e-   initial evidence supplied to remote entity
    Returns: message id used in request -}     
sendReq :: Plc -> Plc -> M.Map Plc Address ->
           Term -> RawEv -> NS.Socket -> IO ()
sendReq pTo pFrom namesFrom t e conn = do
  --logc "inside doSendReq"
  --logc $ "pTo: " ++ (show pTo)
  --nextMID <- liftIO $ doRNG
  let rm = (RequestMessage pTo pFrom namesFrom t e)
  let messageBits = DA.encode rm
  --logc $ "sending doSendReq: " ++ (show rm)
  --logc $ "JSON Request: " ++ (show messageBits)


  let jsonReqInFile = "../demoOutput/jsonReqIn.hs"
      
  SD.removeFile jsonReqInFile `catch` handleExists
  JC.jsonToFile rm jsonReqInFile

  
  NBS.sendAll conn (BL.toStrict messageBits)
  --return nextMID


    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

{-  Send a response to an attestation request
    Params:
      mid- message id (should match message id from corresponding request)
      pTo- intended recipient of response
      e-   resulting evidence gathered  -}
sendResp :: NS.Socket -> Plc -> Plc -> RawEv -> IO ()
sendResp conn pFrom pTo e = do
  --pFrom <- asks me
  let rm = (ResponseMessage pTo pFrom e)
  let messageBits = DA.encode rm
  {-logc $ "sending doSendResp: " ++ (show rm)
  logc $ "JSON Response: " ++ (show messageBits) -}
  --putStrLn $ "JSON Response: " ++ (show messageBits)
  NBS.sendAll conn (BL.toStrict messageBits)

{-  Receive an attestation response
    Params:
      goodMid-   expected message id (from corresponding request id)
      goodPfrom- expected source place
    Returns:  evidence from response message  -}
receiveResp :: NS.Socket -> Plc -> IO ResponseMessage
receiveResp conn goodPfrom = do
  --logc "inside doRecieveResponse"
  
  --p <- asks me
  msg <- NBS.recv conn 2048
  let (val :: Maybe ResponseMessage) = DA.decodeStrict msg
  case val of
      Nothing -> error $ "weird message received: " ++ (show msg)
      Just res -> do
        -- TODO: check mid here?  May be obsolete with socket connection?
        let jsonRespOutFile = "../demoOutput/jsonRespOut.hs"
        SD.removeFile jsonRespOutFile `catch` handleExists
        JC.jsonToFile res jsonRespOutFile
        return res

    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
                          
{-  Receive an attestation request.
    Params:  None (for now, act as a server and respond to all requests)
    Returns (pieces of request message)
      -message id
      -place
      -protocol term
      -initial evidence -}   
receiveReq :: NS.Socket -> IO RequestMessage
receiveReq conn = do
  --logc "inside doRecieveRequest"

  msg <-  NBS.recv conn 2048
  let (val :: Maybe RequestMessage) = DA.decodeStrict msg
  case val of
   Nothing -> error $ "weird message received: " ++ (show msg)
   Just res -> return res
