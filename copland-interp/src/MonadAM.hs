{-  An experimental monad library for managing attestation protocol execution.
    (AM-Attestation Manager)

   Author: Adam Petz
   Date:  04/23/2019
-}

module MonadAM where

import Copland
import CryptoImpl as CI (doNonce)
import Interp (interp,getNameMap)
import MonadCop (Cop_Env(..), runCOP, lookupSecretKeyPath)
import ClientProgArgs (getClientOptions, Client_Options(..))

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B (ByteString, readFile, empty)
import qualified Data.Map as M

{-  The Attestation Manager Monad  -}
type AM = ReaderT AM_Env (StateT AM_St IO)

{-  Read-only environment used during protocol execution.
    Contains:

-}
data AM_Env =
  AM_Env { myPolicy :: Policy
          } deriving (Show)

type Policy = String

{-  State for state monad.
    Contains:
 -}
data AM_St =
  AM_St { am_nonceMap :: M.Map Int B.ByteString,
           am_nonceId  :: Int
         } deriving (Show)

runAM :: AM a -> AM_Env -> AM_St -> IO (a, AM_St)
runAM k env st =
     runStateT (runReaderT k env) st

am_updateNonce :: B.ByteString -> AM Int
am_updateNonce bs = do
  (AM_St m id) <- get
  let newMap = M.insert id bs m
      newId = id + 1
  put (AM_St newMap newId)
  return id

am_genNonce :: AM Ev
am_genNonce = do
  bs <- liftIO $ CI.doNonce
  new_id <- am_updateNonce bs
  --let my_place = 0 -- TODO:  should we make this more general?
  return $ N new_id bs Mt

am_getNonce :: Int -> AM B.ByteString
am_getNonce i = do
  (AM_St m _) <- get
  let maybeVal = M.lookup i m
  case maybeVal of
   Nothing -> return B.empty {- TODO: better error handling -}
   Just val -> return val

am_checkNonce :: Ev -> AM Bool
am_checkNonce e = do
  case e of
   N i bs e' -> do
     goldenVal <- am_getNonce i
     return (goldenVal == bs)

   _ -> return False
   {- TODO: better return type/error handling for non-nonce case? -}

am_runCOP :: T -> Ev -> M.Map Pl Address -> AM Ev
am_runCOP t e m = do
  opts <- liftIO $ getClientOptions
  cop_env <- liftIO $ build_AM_Env opts m
  res <- liftIO $ runCOP (interp t e) cop_env
  return res

build_AM_Env :: Client_Options -> M.Map Pl Address -> IO Cop_Env
build_AM_Env opts nameMap = do

  let b = optSim opts
      d = optDebug opts
      pl = 0 -- TODO:  hardcoded
      
  keyPath <- lookupSecretKeyPath
  return $ Cop_Env b d nameMap keyPath pl
  {- TODO: ok to return place 0, since it will be updated? -}
