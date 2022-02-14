{-  An experimental monad library for managing attestation protocol execution.
    (AM-Attestation Manager)

   Author: Adam Petz
   Date:  04/23/2019
-}

module MonadAM where

import Copland
import BS
import CryptoImpl as CI (doNonce, lookupSecretKeyPath)
import MonadCop (Cop_Env(..), runCOP, COP)
import ClientProgArgs
import StVM (Coq_cvm_st(..))
import qualified DemoStates as DS
import Impl_VM_Extracted (run_cvm')
import Comm (genNameServer)
import Data.List(union)

import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Error
-- import Control.Monad.Trans.MaybeT
import qualified Data.ByteString as B (ByteString, readFile, empty)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)
import qualified Data.Map as M
import qualified Data.Binary as D (encode,decode)

--import GenOptMonad (AM)

{-  The Attestation Manager Monad  -}
--type AM = ReaderT AM_Env (StateT AM_St IO)
type AM = StateT AM_St (ReaderT AM_Env IO)


--type AM = ReaderT AM_Env (StateT AM_St (MaybeT IO))

{-  Read-only environment used during protocol execution.
    Contains:

-}
data AM_Env =
  AM_Env { sig_map :: M.Map Plc ASP_ID,
           hsh_map :: M.Map Plc ASP_ID,
           asp_map :: M.Map (Plc,ASP_ID) ASP_ID
           {-nonce_check_asp :: ASP_ID -}
          } deriving (Show)

type Policy = String

{-  State for state monad.
    Contains:
 -}
data AM_St =
  AM_St { am_nonceMap :: M.Map Int BS,
          {-am_nonceAppraiseMap :: M.Map Int Bool, -}
          am_nonceId  :: Int
         } deriving (Show)

empty_AM_state = AM_St { am_nonceMap =  M.empty,
                         {-am_nonceAppraiseMap = M.empty, -}
                         am_nonceId = 0 }

empty_AM_env = AM_Env {  sig_map = M.empty,
                         hsh_map = M.empty,
                         asp_map = M.empty
                         {-nonce_check_asp = 0-} }

initial_AM_env sigMap hshMap aspMap =
  empty_AM_env { sig_map = sigMap, hsh_map = hshMap, asp_map = aspMap
                   {-nonce_check_asp = nca-} }

runAM :: AM a -> AM_Env -> AM_St -> IO (a, AM_St)
runAM k env st =
     --runStateT (runReaderT k env) st
  runReaderT (runStateT k st) env


am_genNonce :: AM EvC
am_genNonce = do
  bs <- liftIO $ CI.doNonce
  new_id <- am_updateNonce bs
  return $ Coq_evc [bs] (Coq_nn new_id)

am_run_cvm :: Bool -> Bool -> Term -> AM Coq_cvm_st
am_run_cvm simb debugb t = do
  ne <- am_genNonce
  let mypl = 0 -- TODO: make not hardcoded?
      st = (Coq_mk_st ne [] 0 0)
      env = DS.sample_cop_env simb debugb t mypl
  liftIO $ run_cvm' t st env

am_get_sig_asp :: Plc -> AM ASP_ID
am_get_sig_asp p = do
  m <- asks sig_map
  let maybeId = M.lookup p m
  case maybeId of
   Just i -> return i
   Nothing -> error $ "appraisal ASP_ID for SIG at place " ++ (show p) ++ " not registered."

am_get_hsh_asp :: Plc -> AM ASP_ID
am_get_hsh_asp p = do
  m <- asks hsh_map
  let maybeId = M.lookup p m
  case maybeId of
   Just i -> return i
   Nothing -> error $ "appraisal asp for HSH at place " ++ (show p) ++ " not registered."

am_get_asp_asp :: Plc -> ASP_ID -> AM ASP_ID
am_get_asp_asp p i = do
  m <- asks asp_map
  let maybeId = M.lookup (p,i) m
  case maybeId of
   Just newI -> return newI
   Nothing -> error $ "appraisal asp for ASP_ID " ++ (show i) ++ " at place " ++ (show p) ++ " not registered."

am_updateNonce :: B.ByteString -> AM Int
am_updateNonce bs = do
  (AM_St m id) <- get
  let newMap = M.insert id bs m
      newId = id + 1
  put (AM_St newMap newId)
  return id

{-
am_genNonce :: EvidenceC  -> AM EvidenceC 
am_genNonce e = do
  bs <- liftIO $ CI.doNonce
  new_id <- am_updateNonce bs
  --let my_place = 0 -- TODO:  should we make this more general?
  return $ N new_id bs e
-}

am_getNonce :: Int -> AM BS
am_getNonce i = do
  (AM_St m _) <- get
  let maybeVal = M.lookup i m
  case maybeVal of
   Nothing ->
     error $ "Nonce with ID '" ++ (show i) ++ "' not initialized in AM State"
   Just val -> return val

am_appraise_nonce :: Int -> BS -> AM ()
am_appraise_nonce i bs = do
  (AM_St x y) <- get
  goldenVal <- am_getNonce i
  let b = (bs == goldenVal)
      --newAmap = M.insert i b amap
  put (AM_St x y)


nameMap_from_term :: Term -> IO (M.Map Plc Address)
nameMap_from_term t = do
  let places = getPlaces t
  res <- genNameServer places
  return res

getPlaces :: Term -> [Plc]
getPlaces t = getPlaces' t []

getPlaces' :: Term -> [Plc] -> [Plc]
getPlaces' t pls =
  case t of
   Coq_att p t' -> union [p] (getPlaces' t' pls)
   Coq_lseq t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bseq _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   Coq_bpar _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   _ -> pls


  

{-
am_get_app_nonces :: AM (M.Map Int Bool)
am_get_app_nonces = do
  gets am_nonceAppraiseMap
-}

{-
am_clear_app_nonces :: AM ()
am_clear_app_nonces = do
  (AM_St x _ y) <- get
  put (AM_St x (M.empty) y)
-}

{-
am_get_app_nonce_bools :: AM [Bool]
am_get_app_nonce_bools = do
  m <- am_get_app_nonces
  liftIO $ putStrLn $ "nonce bool map: " ++ (show m)
  let ls = M.toList m
  return $ fmap snd ls

am_get_app_nonce_bool :: AM Bool
am_get_app_nonce_bool = do
  bs <- am_get_app_nonce_bools
  return (and bs)
-}

{-
am_checkNonce :: Ev -> AM Bool
am_checkNonce e = do
  case e of
   N i bs e' -> do
     goldenVal <- am_getNonce i
     return (goldenVal == bs)

   _ -> return False
-}
   {- TODO: better return type/error handling for non-nonce case? -}


{-
-- TODO:  refactor to fold
ev_nonce_list' :: EvidenceC  -> [(Int,BS)] -> [(Int,BS)]
ev_nonce_list' e ls =
  case e of
   N i bs e' -> (i,bs) : (ev_nonce_list' e' ls)
   _ -> ls

ev_nonce_list :: EvidenceC  -> [(Int,BS)]
ev_nonce_list e = ev_nonce_list' e []
-}

{-
n_to_term :: (Int,BS) -> AM Term
n_to_term (i,bs) = do
  n_asp <- asks nonce_check_asp
  let arg1 = (show i)
      arg2 = (show bs) in
  {-let arg1 = BL.toStrict $ D.encode i
      arg2 = bs in -}
   return $ ASPT $ undefined --ASPC n_asp [arg1,arg2]
-}

check_nonces' :: [(Int,BS)] -> AM [Bool]
check_nonces' ls =
  mapM f ls
  where f :: (Int,BS) -> AM Bool
        f (i,bs) = do
          goldenVal <- am_getNonce i
          return (goldenVal == bs)

check_nonces :: [(Int,BS)] -> AM Bool
check_nonces ls = do
  bools <- check_nonces' ls
  return (and bools)

{-
check_nonces_ev :: EvidenceC  -> AM Bool
check_nonces_ev e = do
  let ls = ev_nonce_list e
  check_nonces ls
-}
  

{-
nlist_to_term :: [(Int,BS)] -> AM Term
nlist_to_term ls =
  case ls of
   [] -> return $ ASPT $ CPY
   (p : ls') -> do
     v <- n_to_term p
     rest <- (nlist_to_term ls')
     return $ LN v rest
-}

{-
ev_nonce_term :: EvidenceC -> AM Term
ev_nonce_term e = do
  let ls = ev_nonce_list e
  nlist_to_term ls
-}

{-
gen_evt' :: T -> T -> Ev -> Pl -> Ev_T
gen_evt' t init_t e p =
  case t of
   ASP i args -> do
     case e of
      U _ _ bs e' ->
        {-let et' = gen_evt' -}
        
{-
        do
        app_id <- am_get_asp_asp p i
        let res = ((ASP app_id ((show bs) : args)), e')
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res -}
      _ -> error "evidence mismath on ASP-U"
   SIG -> do
     case e of
      G bs e' -> do
        sig_id <- am_get_sig_asp p
        let evBits = encodeEv e' --BL.toStrict (DA.encode e)
            evBitsArg = show evBits
            sigArg = show bs
            res = ((ASP sig_id [evBitsArg,sigArg]), e')
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res -- TODO: custom args here?
      _ -> error "evidence mismath on SIG-G"
   HSH -> do
     case e of
      H bs -> do
        hsh_id <- am_get_hsh_asp p
        return $ ((ASP hsh_id []), e) -- TODO: custom args here?  e or Mt?
      _ -> error "evidence mismath on HSH-H"
   CPY -> do
     let res = (CPY,e)
     liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
     return res -- TODO: is this acceptable?
   AT q t' -> do
     gen_appraisal_term' t' q e -- TODO: e or Mt?
     {-(t'_res_term, _) <- gen_appraisal_term' t' q e -- TODO: e or Mt?
     return (t'_res_term, e) -}
   LN t1 t2 -> do
     (t2',e2) <- (gen_appraisal_term' t2 p e)
     liftIO $ putStrLn $ "E2: " ++ (show e2)
     (t1',e1) <- (gen_appraisal_term' t1 p e2)
     return $ ((BRP (NONE,NONE) t1' t2'),e1) -- TODO: (NONE,NONE)?  e1?
   BRS (_,_) t1 t2 -> do
     case e of
      SS e1 e2 -> do
        {-let e1 = allnone sp1 e
            e2 = allnone sp2 e   -}
        (t1',_) <- (gen_appraisal_term' t1 p e1)
        (t2',_) <- (gen_appraisal_term' t2 p e2)
        let res = ((BRP (NONE,NONE) t1' t2'),Mt)
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res -- TODO: BRP?  (NONE,NONE)?  Mt?
      _ -> error "evidence mismath on BRS-SS"
   BRP (_,_) t1 t2 -> do
    case e of
     SS e1 e2 -> do -- TODO:  change this back to PP once parallel supported
    {- let e1 = allnone sp1 e
         e2 = allnone sp2 e -}  
       (t1',_) <- (gen_appraisal_term' t1 p e1)
       (t2',_) <- (gen_appraisal_term' t2 p e2)
       return $ ((BRP (NONE,NONE) t1' t2'),Mt) -- TODO: (NONE,NONE)?  Mt?
     _ -> error "evidence mismath on BRP-PP"
-}

{-
gen_from_ev' :: Ev -> AM T
gen_from_ev' e = return CPY
-}


{-
gen_from_ev' :: EvidenceC -> Evidence -> AM Term
gen_from_ev' e et = undefined
-}


{-
gen_from_ev' :: EvidenceC -> Evidence -> AM Term
gen_from_ev' e et = do
  --liftIO $ putStrLn $ "\nterm in gen_appriasel_term': \n" ++ (prettyT t) ++ "\n"
  liftIO $ putStrLn $ "\nevidence in gen_appriasal_term': \n" ++ (prettyEv e) ++ "\n"
  case e of
   N i bs e' ->
     case et of
      Nt i_t e'_t -> do
        am_appraise_nonce i_t bs
        gen_from_ev' e' e'_t
        {- case (i == i_t) of
         True -> do
           am_appraise_nonce i bs
           gen_from_ev' e' e'_t
         False -> error "nonce id mismatch" -}
      _ -> error "evidence mismath on N Nt"  
   Mt ->
     case et of
      Mtt -> return (ASPT CPY)
      _ -> error "evidence mismath on Mt Mtt"
   U i args bs e' ->
     case et of
      Ut p (ASP_PARAMSC i_t args_t _ _) e'_t -> do
        app_id <- am_get_asp_asp p i_t
        t2 <- gen_from_ev' e' e'_t
        let t1 = ASPT $ undefined --(ASPC app_id ((show bs) : args_t))
        let res = BRP (NONE,NONE) t1 t2
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT res) ++ "\n"
        return res
      _ -> error "evidence mismath on U Ut"         
   G bs e' ->
     case et of
      Gt p e'_t -> do
        sig_id <- am_get_sig_asp p
        let evBits = encodeEv e' --BL.toStrict (DA.encode e)
            evBitsArg = show evBits
            sigArg = show bs
        t2 <- gen_from_ev' e' e'_t
        let t1 = ASPT $ undefined --(ASPC sig_id [evBitsArg,sigArg])
            res = BRP (NONE,NONE) t1 t2
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT res) ++ "\n"
        return $ res -- TODO: custom args here?
      _ -> error "evidence mismath on G Gt"
   H bs ->
     case et of
      Ht p _ -> do
        hsh_id <- am_get_hsh_asp p
        let hshArg = show bs
        return $ ASPT $ undefined --(ASPC hsh_id [hshArg]) -- TODO: custom args here?  e or Mt?
      _ -> error "evidence mismath on H Ht"
   SS e1 e2 ->
     case et of
      SSt e1_t e2_t -> do
     {-let e1 = allnone sp1 e
            e2 = allnone sp2 e   -}
        t1' <- (gen_from_ev' e1 e1_t)
        t2' <- (gen_from_ev' e2 e2_t)
        let res = BRP (NONE,NONE) t1' t2'
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT res) ++ "\n"
        return $ res -- TODO: BRP?  (NONE,NONE)?  Mt?
      _ -> error "evidence mismath on SS SSt"

   PP e1 e2 ->
     case et of
      PPt e1_t e2_t -> do
     {-let e1 = allnone sp1 e
            e2 = allnone sp2 e   -}
        t1' <- (gen_from_ev' e1 e1_t)
        t2' <- (gen_from_ev' e2 e2_t)
        let res = BRP (NONE,NONE) t1' t2'
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT res) ++ "\n"
        return $ res -- TODO: BRP?  (NONE,NONE)?  Mt?
      _ -> error "evidence mismath on P PPt"
        
  where allnone sp e =
          case sp of
           ALL -> e
           NONE -> Mt


-}




        
{-
gen_appraisal_term' :: T -> Pl -> Ev -> AM (T,Ev)
gen_appraisal_term' t p e = do
  liftIO $ putStrLn $ "\nterm in gen_appriasel_term': \n" ++ (prettyT t) ++ "\n"
  liftIO $ putStrLn $ "\nevidence in gen_appriasel_term': \n" ++ (prettyEv e) ++ "\n"
  case t of
   ASP i args -> do
     case e of
      U _ _ args bs e' -> do
        app_id <- am_get_asp_asp p i
        let res = ((ASP app_id ((show bs) : args)), e')
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res
      _ -> error "evidence mismath on ASP-U"
   SIG -> do
     case e of
      G _ bs e' -> do
        sig_id <- am_get_sig_asp p
        let evBits = encodeEv e' --BL.toStrict (DA.encode e)
            evBitsArg = show evBits
            sigArg = show bs
            res = ((ASP sig_id [evBitsArg,sigArg]), e')
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res -- TODO: custom args here?
      _ -> error "evidence mismath on SIG-G"
   HSH -> do
     case e of
      H _ bs -> do
        hsh_id <- am_get_hsh_asp p
        return $ ((ASP hsh_id []), e) -- TODO: custom args here?  e or Mt?
      _ -> error "evidence mismath on HSH-H"
   CPY -> do
     let res = (CPY,e)
     liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
     return res -- TODO: is this acceptable?
   AT q t' -> do
     gen_appraisal_term' t' q e -- TODO: e or Mt?
     {-(t'_res_term, _) <- gen_appraisal_term' t' q e -- TODO: e or Mt?
     return (t'_res_term, e) -}
   LN t1 t2 -> do
     (t2',e2) <- (gen_appraisal_term' t2 p e)
     liftIO $ putStrLn $ "E2: " ++ (show e2)
     (t1',e1) <- (gen_appraisal_term' t1 p e2)
     return $ ((BRP (NONE,NONE) t1' t2'),e1) -- TODO: (NONE,NONE)?  e1?
   BRS (_,_) t1 t2 -> do
     case e of
      SS e1 e2 -> do
        {-let e1 = allnone sp1 e
            e2 = allnone sp2 e   -}
        (t1',_) <- (gen_appraisal_term' t1 p e1)
        (t2',_) <- (gen_appraisal_term' t2 p e2)
        let res = ((BRP (NONE,NONE) t1' t2'),Mt)
        liftIO $ putStrLn $ "\nreterming in gen_appriasel_term': \n" ++
          (prettyT (fst res)) ++ "\n" ++ (prettyEv (snd res)) ++ "\n"
        return $ res -- TODO: BRP?  (NONE,NONE)?  Mt?
      _ -> error "evidence mismath on BRS-SS"
   BRP (_,_) t1 t2 -> do
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


t_to_evt'' :: ASP -> Plc -> Evidence -> Evidence
t_to_evt'' t p e =
  case t of
    ASPC params -> Ut p params e
    SIG -> Gt p e
    HSH -> Ht p e
    CPY -> e


t_to_evt' :: Term -> Plc -> Evidence -> Evidence
t_to_evt' t p e =
  case t of
   ASPT a -> t_to_evt'' a p e
   AT q t' -> t_to_evt' t' q e
   LN t1 t2 ->
     let e1t = t_to_evt' t1 p e in
     t_to_evt' t2 p e1t
   BRS (sp1,sp2) t1 t2 ->
     let e1_init = allnone sp1 e in
     let e2_init = allnone sp2 e in
     let e1 = t_to_evt' t1 p e1_init in
     let e2 = t_to_evt' t2 p e2_init in
     SSt e1 e2
   BRP (sp1,sp2) t1 t2 ->
     let e1_init = allnone sp1 e in
     let e2_init = allnone sp2 e in
     let e1 = t_to_evt' t1 p e1_init in
     let e2 = t_to_evt' t2 p e2_init in
     SSt e1 e2 -- TODO: change this to PPt once BRP implemented
         
  where allnone sp e =
          case sp of
           ALL -> e
           NONE -> Mtt
    

t_to_evt :: Term -> Evidence -> Evidence
t_to_evt t e = t_to_evt' t 0 e

gen_appraisal_term :: Term -> {-Ev ->-} EvidenceC -> Evidence -> AM Term
gen_appraisal_term t {-initEv-} resEv initEv_T = do
  am_clear_app_nonces
  --t1 <- ev_nonce_term initEv
  let resEv_t = t_to_evt t initEv_T
  t2 <- gen_from_ev' resEv resEv_t
  --return $ LN t1 t2
  return t2



appraise_ev :: EvidenceC -> [Bool]
appraise_ev e =
  case e of
   SS e1 e2 -> -- TODO:  this will become PP evidence once AVM supports BRP
     let e1bs = appraise_ev e1 in
     let e2bs = appraise_ev e2 in
      e1bs ++ e2bs
   U _ _ bs e' ->
     let e'bs = appraise_ev e' in
     let aBool= D.decode (BL.fromStrict bs) in
      aBool : e'bs
   Mt -> []
   _ -> error "shouldn't happen because of gen_appraisal_term definition"
-}
