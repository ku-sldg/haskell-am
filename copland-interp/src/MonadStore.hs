{-  An experimental monad library for managing a shared memory store.

   Author: Adam Petz
   Date:  12/16/2020
-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonadStore where

import Copland
import MonadCop
--import ServerAppUtil

import System.Environment (lookupEnv)
import Control.Monad.Reader
--import Control.Monad.State.Lazy
import Control.Monad.Trans(liftIO)
import Numeric.Natural
import qualified Data.Map as M
import qualified Data.Set as S 

import Control.Concurrent.STM


{-  Environment required by the Store.
    Contains:

-}

data  Store_Env = Store_Env
  { real_index_map :: TMVar (M.Map (VM_ID,Natural) Natural)
  , st_store :: TMVar (M.Map Natural (TMVar Ev))
  , available_indices :: TMVar (S.Set Natural)
  }

{-
emptyState = Store_Env { st_store_map = M.empty
                      , st_store = M.empty }

initial_Store_env ev =
  emptyState { st_store_map = ev }
-}

type StoreM = ReaderT Store_Env IO


{-
put_store_at :: Natural -> Ev -> VM ()
put_store_at n e = do
  st <- get
  let store = st_store st
  let store' = M.insert n e store
  put (st {st_store = store'})
  
  

get_store_at :: Natural -> VM Ev
get_store_at n = do
  store' <- gets st_store
  let maybeEv = M.lookup n store'
  case maybeEv of
    Just e -> return e
    Nothing -> error $ "Failed to access st_store at index: " ++ (show n) ++ "."



get_asp_socket :: ASP_ID -> VM String
get_asp_socket i = do
  socks <- gets st_aspSockets
  let maybeSock = M.lookup i socks
  case maybeSock of
   Just s -> return s
   Nothing -> error $ "No socket registered for ASP " ++ (show i) ++ "."


get_ev :: VM Ev
get_ev = gets st_ev

put_ev :: Ev -> VM ()
put_ev newEv = modify (\s -> s{ st_ev = newEv })
-}

{-
get_nextIndex :: VM Int
get_nextIndex = do
  s <- get
  let index = (st_index s) + 1
  put s{ st_index = index}
  return index

push_stackm :: Ev -> VM ()
push_stackm e = modify (\s -> s{st_stack = stackPush (st_stack s) e })

pop_stackm :: VM Ev
pop_stackm = do
  s <- get
  case stackPop (st_stack s) of
    Just (newstack, top) -> do {put s{st_stack = newstack}; return top}
    _ ->  fail "VM state stack is empty"
-}

{- should be just "asks serverSocket", when serverSocket is moved to the Environment -}

{-
get_serverSocket :: VM String
get_serverSocket = do
  s <- get
  return $ st_serverSocket s

{- should be just "asks sigSocket", when sigSocket is moved to the Environment -}
get_sigSocket :: VM String
get_sigSocket = do
  s <- get
  return $ st_sigSocket s
-}
