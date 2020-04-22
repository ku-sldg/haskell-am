{-  Executable that acts as the top-level client, performing the first request(s) of an attestation protocol execution.  Sequences execution of Copland phrases, collects results, and optionally performs appraisal.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland
import MonadCop (lookupSecretKeyBytesIO, lookupSecretKeyPath)
import MonadAM
import ExecCopland
import MonadVM
import Interp (spawn_the_servers, getNameMap)
import Comm (genNameServer)
import ClientProgArgs (getClientOptions, Client_Options(..))
--import qualified Appraise as APP (appraiseUsm)
import qualified CryptoImpl as CI (doHashFile)

import Control.Monad.Trans(liftIO)
import Data.List(union)
import Text.Read(readMaybe)
import Crypto.Sign.Ed25519 (SecretKey(..), verify, toPublicKey)
import qualified Data.Map as M
import qualified Control.Concurrent as CC (threadDelay)
import qualified Data.ByteString as B (empty, writeFile)


main :: IO ()
main = do
  opts <- liftIO $ getClientOptions
  let provBool = optProv opts
  case provBool of
   True -> provision
   False -> am_main am_proto_1 am_state_init

{- This is a hard-coded initial state for demo/testing purposes.
   TODO:  Need to make this user-facing, provide an external registration process for ASP components.  -}
am_state_init :: AM_St
am_state_init = 
  let -- Maps place to the asp_id that checks signatures from that place.
      app_sig_map = M.fromList [(0,41)]
      -- Similar map for hash appraisals,
      app_hsh_map = M.empty
      -- Maps a place/asp_id pair to the asp_id that can appraise it.
      app_asp_map = M.fromList [((0, 1),42)]
      -- asp_id for (appraisal) asp that checks nonces
      app_nonceCheckAsp = 0 in
        initial_AM_state app_sig_map app_hsh_map app_asp_map app_nonceCheckAsp
        
am_main :: AM Ev -> AM_St -> IO ()
am_main proto init = do
  (resEv, resState) <- runAM_with_st proto init
  return ()

runAM_with_st :: AM Ev -> AM_St -> IO (Ev, AM_St)
runAM_with_st am_computation am_st = do
  let fresh_AM_Env = (AM_Env "")
      fresh_AM_St = am_st
  runAM am_computation fresh_AM_Env fresh_AM_St


runAM_fresh :: AM Ev -> IO (Ev, AM_St)
runAM_fresh am_computation = do
  let fresh_AM_Env = (AM_Env "")
      fresh_AM_St = (AM_St M.empty 0 M.empty M.empty M.empty 0)
  runAM am_computation fresh_AM_Env fresh_AM_St

nameMap_from_term :: T -> IO (M.Map Pl Address)
nameMap_from_term t = do
  let places = getPlaces t
  res <- genNameServer places
  return res

am_proto_1 :: AM Ev
am_proto_1 = do
  opts <- liftIO $ getClientOptions
  let termFile = optTermIn opts
      evFile = optEvIn opts
      namesFile = optNames opts
      spawnServers = optSpawn opts
      spawnSimBool = optSpawnSim opts
      spawnDebugBool = optSpawnDebug opts
      appraiseBool = optApp opts
      compileBool = optCompile opts

  (t,ev) <- liftIO $ get_term_ev termFile evFile

  {- Uncomment to use generated nonce as initial evidence 
  n <- am_genNonce
  let ev = n
  -}

  let places = getPlaces t

  nm <- liftIO $ getNameMap namesFile places

  case spawnServers of
   True -> do
     liftIO $ spawn_the_servers nm spawnSimBool spawnDebugBool
     liftIO $ CC.threadDelay 10000
   False -> return ()

  -- if client compiles the received copland term,
  -- and executes the generated sequence of copland instructions.
  resEv <- case compileBool of
             True -> liftIO $ run_vm_t t ev nm
             -- or simply interprets the received copland term
             False -> am_runCOP t ev nm

  case appraiseBool of
   True -> do
     app_term <- gen_appraisal_term t 0 ev resEv -- TODO: 0 place ok?
     liftIO $ putStrLn $ "app_term: " ++ (show app_term)
     app_ev <- liftIO $ run_vm_t app_term Mt nm -- TODO: Mt evidence ok?
     --b <- appraise_proto_1 resEv
     liftIO $ putStrLn $ "appraisal result: " ++ (show app_ev)
     let boolList = appraise_ev app_ev
     let appBoolResult = and boolList
     liftIO $ putStrLn $ "bool list: " ++ (show boolList)
     liftIO $ putStrLn $ "Appraisal result: " ++ (show appBoolResult)
   False -> return ()

  liftIO $ after_output t ev resEv
  return resEv

after_output :: T -> Ev -> Ev -> IO ()
after_output t ev resEv = do
  opts <- getClientOptions
  let jsonFlag = optJson opts
      outp = optOut opts
      protoInFile = "../demoOutput/protoIn.hs"
      evOutFile = "../demoOutput/protoOut.hs"
      pString = (prettyT t) ++ "\n\n" ++ (prettyEv ev)

  writeFile protoInFile pString
  case jsonFlag of
   True -> jsonOut t ev resEv
   False -> return ()

  case outp of
   "" -> do
     print_write_ev_result resEv "../demoOutput/protoOut.hs"
   fp -> do
     print_write_ev_result resEv fp

   where
     print_write_ev_result :: Ev -> FilePath -> IO ()
     print_write_ev_result resEv fp = do
       putStrLn $ "\n" ++ "Evidence Result: " ++ "\n" ++ (prettyEv resEv) ++ "\n"
       writeFile fp (prettyEv resEv)

get_term_ev :: FilePath -> FilePath -> IO (T, Ev)
get_term_ev inp einp = do
  t <-
    case inp of
     "" -> return proto1
     _ -> getTerm inp
  ev <-
    case einp of
     "" -> return Mt
     _ -> getEv einp

  before_output t ev
  return (t,ev)

  where
    proto1 = CPY
             {-AT 1
             (LN
              (BRP (ALL,NONE) CPY (ASP 1 ["target.txt"]))
              SIG) -}
    before_output :: T -> Ev -> IO ()
    before_output t ev = do
      putStrLn $ "\n" ++ "Protocol Executed(Also in demoOutput/protoIn.hs): \n" ++ (prettyT t)
      putStrLn $ "\n" ++ "Initial Evidence: \n" ++ (prettyEv ev) ++ "\n"

-- Attempt to read a value from the FIRST LINE of a file.
-- Output a type(and filepath)-specific error message upon failure.
readFileGen :: Read a => FilePath -> String -> IO a
readFileGen fp typeString = do
      s <- readFile fp
      let ss = lines s
      let maybeT = readMaybe (head ss)
      case maybeT of
       Just t -> return t
       _ -> error $ "Failed to parse " ++ typeString ++ " from file: " ++ fp
       
getTerm :: FilePath -> IO T
getTerm fp = readFileGen fp "T"

getEv :: FilePath -> IO Ev
getEv fp = readFileGen fp "Ev"

{- Hard-coded provisioning.
   TODO: Make this user-facing.
         Probably calls for its own provisioning executable + CLI.  -}
provision :: IO ()
provision = do
  bits <- CI.doHashFile "../target.txt"
  B.writeFile "goldenInputBits.txt" bits

  {-bits <- CI.doHashFile "../kimTarget.txt"
  B.writeFile "goldenKimBits.txt" bits-}

getPlaces :: T -> [Pl]
getPlaces t = getPlaces' t []

getPlaces' :: T -> [Pl] -> [Pl]
getPlaces' t pls =
  case t of
   AT p t' -> union [p] (getPlaces' t' pls)
   LN t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   BRS _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   BRP _ t1 t2 ->
     let ls = getPlaces' t1 pls in
     let rs = getPlaces' t2 pls in
     union ls rs
   _ -> pls



{-
appraise_proto_1 :: Ev -> AM Bool
appraise_proto_1 e = do
  let (G sigVal e'@(PP n@(N 0 nonceVal (Mt))
           (U 1 args {-["target.txt"]-} hashVal (Mt)))
       ) = e

  kp <- liftIO $ lookupSecretKeyPath
  priKeyBits <- liftIO $ lookupSecretKeyBytesIO kp

  let evBits = encodeEv e'
  let priKey = SecretKey priKeyBits
      pubKey = toPublicKey priKey  --TODO: generalize public key management
      sigResult = verify pubKey sigVal

  liftIO $ putStrLn $ "Sig Check: " ++ (show sigResult)
  (usmCheck,goldenHash) <- liftIO $ APP.appraiseUsm 1 1 args hashVal

  liftIO $ putStrLn $ "USM Check: " ++ (show usmCheck)
  nonceCheck <- am_checkNonce n

  liftIO $ putStrLn $ "Nonce Check: " ++ (show nonceCheck)
  return (sigResult && usmCheck && nonceCheck)
-}
