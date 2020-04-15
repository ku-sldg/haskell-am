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
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Map as M
import qualified Data.Binary as D (encode)

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
           am_nonceId  :: Int,
           sig_map :: M.Map Pl ASP_ID,
           hsh_map :: M.Map Pl ASP_ID,
           asp_map :: M.Map (Pl,ASP_ID) ASP_ID,
           nonce_check_asp :: ASP_ID
         } deriving (Show)

runAM :: AM a -> AM_Env -> AM_St -> IO (a, AM_St)
runAM k env st =
     runStateT (runReaderT k env) st

am_get_sig_asp :: Pl -> AM ASP_ID
am_get_sig_asp p = do
  m <- gets sig_map
  let maybeId = M.lookup p m
  case maybeId of
   Just i -> return i
   Nothing -> error $ "appraisal ASP_ID for SIG at place " ++ (show p) ++ " not registered."

am_get_hsh_asp :: Pl -> AM ASP_ID
am_get_hsh_asp p = do
  m <- gets hsh_map
  let maybeId = M.lookup p m
  case maybeId of
   Just i -> return i
   Nothing -> error $ "appraisal asp for HSH at place " ++ (show p) ++ " not registered."

am_get_asp_asp :: Pl -> ASP_ID -> AM ASP_ID
am_get_asp_asp p i = do
  m <- gets asp_map
  let maybeId = M.lookup (p,i) m
  case maybeId of
   Just newI -> return newI
   Nothing -> error $ "appraisal asp for ASP_ID " ++ (show i) ++ " at place " ++ (show p) ++ " not registered."

am_updateNonce :: B.ByteString -> AM Int
am_updateNonce bs = do
  (AM_St m id w x y z) <- get
  let newMap = M.insert id bs m
      newId = id + 1
  put (AM_St newMap newId w x y z)
  return id

am_genNonce :: AM Ev
am_genNonce = do
  bs <- liftIO $ CI.doNonce
  new_id <- am_updateNonce bs
  --let my_place = 0 -- TODO:  should we make this more general?
  return $ N new_id bs Mt

am_getNonce :: Int -> AM B.ByteString
am_getNonce i = do
  (AM_St m _ _ _ _ _) <- get
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


{-
-- TODO: incorporate these as maps in the appraisal monad 
sigMap :: Pl -> ASP_ID
sigMap p = undefined

hshMap :: Pl -> ASP_ID
hshMap p = undefined

aspMap :: ASP_ID -> Pl -> ASP_ID
aspMap i = undefined
-}


-- TODO:  refactor to fold
ev_nonce_list' :: Ev -> [(Int,BS)] -> [(Int,BS)]
ev_nonce_list' e ls =
  case e of
   N i bs e' -> (i,bs) : (ev_nonce_list' e' ls)
   _ -> ls

ev_nonce_list :: Ev -> [(Int,BS)]
ev_nonce_list e = ev_nonce_list' e []

n_to_term :: (Int,BS) -> AM T
n_to_term (i,bs) = do
  n_asp <- gets nonce_check_asp
  let arg1 = BL.toStrict $ D.encode i
      arg2 = bs in
   return $ ASP n_asp [arg1,arg2]
   
  

nlist_to_term :: [(Int,BS)] -> AM T
nlist_to_term ls =
  case ls of
   [] -> return CPY
   (p : ls') -> do
     v <- n_to_term p
     rest <- (nlist_to_term ls')
     return $ LN v rest

{-
gen_appraisal_term' :: T -> Pl -> Ev -> T
gen_appraisal_term' t p e =
  let t_ev = (nlist_to_term (ev_nonce_list e)) in
  let res' =
        case t of
         ASP i args -> ASP (aspMap i p) args
         SIG -> ASP (sigMap p) [] -- TODO: custom args here?
         HSH -> ASP (hshMap p) [] -- TODO: custom args here?
         CPY -> CPY -- TODO: is this acceptable?
         AT q t' -> gen_appraisal_term' t' q e
         LN t1 t2 ->
           let t1' = (gen_appraisal_term' t1 p Mt) in -- TODO: Mt ev?
           let t2' = (gen_appraisal_term' t2 p Mt) in
            BRP (NONE,NONE) t1' t2' -- TODO: (NONE,NONE) ?
         BRS (sp1,sp2) t1 t2 ->
           let t1' = (gen_appraisal_term' t1 p Mt) in -- TODO: Mt ev?
           let t2' = (gen_appraisal_term' t2 p Mt) in
            BRP (sp1,sp2) t1' t2' -- TODO: (sp1,sp2) ?
         BRP (sp1,sp2) t1 t2 ->
           let t1' = (gen_appraisal_term' t1 p Mt) in -- TODO: Mt ev?
           let t2' = (gen_appraisal_term' t2 p Mt) in
            BRP (sp1,sp2) t1' t2' -- TODO: (sp1,sp2) ?
        in
   LN t_ev res'
-}




   

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
