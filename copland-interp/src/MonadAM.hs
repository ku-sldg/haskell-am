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

empty_AM_state = (AM_St M.empty 0 M.empty M.empty M.empty 0)

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
  let arg1 = (show i)
      arg2 = (show bs) in
  {-let arg1 = BL.toStrict $ D.encode i
      arg2 = bs in -}
   return $ ASP n_asp [arg1,arg2]
   
  

nlist_to_term :: [(Int,BS)] -> AM T
nlist_to_term ls =
  case ls of
   [] -> return CPY
   (p : ls') -> do
     v <- n_to_term p
     rest <- (nlist_to_term ls')
     return $ LN v rest

ev_nonce_term :: Ev -> AM T
ev_nonce_term e = do
  let ls = ev_nonce_list e
  nlist_to_term ls
        

gen_appraisal_term' :: T -> Pl -> Ev -> AM (T,Ev)
gen_appraisal_term' t p e = do
  liftIO $ putStrLn $ "evidence in gen_appriasel_term': " ++ (show e)
  case t of
   ASP i args -> do
     case e of
      U _ args bs e' -> do
        app_id <- am_get_asp_asp p i
        return $ ((ASP app_id ((show bs) : args)), e')
      _ -> error "evidence mismath on ASP-U"
   SIG -> do
     case e of
      G bs e' -> do
        sig_id <- am_get_sig_asp p
        return $ ((ASP sig_id []), e') -- TODO: custom args here?
      _ -> error "evidence mismath on SIG-G"
   HSH -> do
     case e of
      H bs -> do
        hsh_id <- am_get_hsh_asp p
        return $ ((ASP hsh_id []), e) -- TODO: custom args here?  e or Mt?
      _ -> error "evidence mismath on HSH-H"
   CPY -> return (CPY,e) -- TODO: is this acceptable?
   AT q t' -> gen_appraisal_term' t' q e -- TODO: e or Mt?
   LN t1 t2 -> do
     (t2',e2) <- (gen_appraisal_term' t2 p e)
     (t1',e1) <- (gen_appraisal_term' t1 p e2)
     return $ ((BRP (NONE,NONE) t1' t2'),e1) -- TODO: (NONE,NONE)?  e1?
   BRS (_,_) t1 t2 -> do
     case e of
      SS e1 e2 -> do
     {-let e1 = allnone sp1 e
         e2 = allnone sp2 e  -}   
        (t1',_) <- (gen_appraisal_term' t1 p e1)
        (t2',_) <- (gen_appraisal_term' t2 p e2)
        return $ ((BRP (NONE,NONE) t1' t2'),Mt) -- TODO: BRP?  (NONE,NONE)?  Mt?
      _ -> error "evidence mismath on BRS-SS"
   BRP (sp1,sp2) t1 t2 -> do
    case e of
     PP e1 e2 -> do
    {- let e1 = allnone sp1 e
         e2 = allnone sp2 e -}  
       (t1',_) <- (gen_appraisal_term' t1 p e1)
       (t2',_) <- (gen_appraisal_term' t2 p e2)
       return $ ((BRP (NONE,NONE) t1' t2'),Mt) -- TODO: (NONE,NONE)?  Mt?
     _ -> error "evidence mismath on BRP-PP"


     {-
     t1' <- (gen_appraisal_term' t1 p e)
     t2' <- (gen_appraisal_term' t2 p e)
     return $ BRP (sp1,sp2) t1' t2' -- TODO: (sp1,sp2) ?
-}

  where allnone sp e =
          case sp of
           ALL -> e
           NONE -> Mt


gen_appraisal_term :: T -> Pl -> Ev -> Ev -> AM T
gen_appraisal_term t p initEv resEv = do
  t1 <- ev_nonce_term initEv
  (t2,_) <- gen_appraisal_term' t p resEv
  return $ LN t1 t2



   

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
