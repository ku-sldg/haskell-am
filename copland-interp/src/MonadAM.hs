{-  An experimental monad library for managing attestation protocol execution.
    (AM-Attestation Manager)

   Author: Adam Petz
   Date:  04/23/2019
-}

module MonadAM where

import Copland
import CryptoImpl as CI (doNonce)
--import Interp (interp,getNameMap)
import MonadCop (Cop_Env(..), runCOP, lookupSecretKeyPath, COP, build_Cop_Env)
import ClientProgArgs

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B (ByteString, readFile, empty)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)
import qualified Data.Map as M
import qualified Data.Binary as D (encode,decode)

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

empty_AM_state = AM_St { am_nonceMap =  M.empty,
                         am_nonceId = 0,
                         sig_map = M.empty,
                         hsh_map = M.empty,
                         asp_map = M.empty,
                         nonce_check_asp = 0 }

initial_AM_state sigMap hshMap aspMap nca =
  empty_AM_state { sig_map = sigMap, hsh_map = hshMap, asp_map = aspMap,
                   nonce_check_asp = nca }

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
        let evBits = encodeEv e' --BL.toStrict (DA.encode e)
            evBitsArg = show evBits
            sigArg = show bs
        return $ ((ASP sig_id [evBitsArg,sigArg]), e') -- TODO: custom args here?
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
     SS e1 e2 -> do -- TODO:  change this back to PP once parallel supported
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



appraise_ev :: Ev -> [Bool]
appraise_ev e =
  case e of
   SS e1 e2 ->
     let e1bs = appraise_ev e1 in
     let e2bs = appraise_ev e2 in
      e1bs ++ e2bs
   U _ _ bs e' ->
     let e'bs = appraise_ev e' in
     let aBool= D.decode (BL.fromStrict bs) in
      aBool : e'bs
   Mt -> []
   _ -> error "shouldn't happen because of gen_appraisal_term definition"
