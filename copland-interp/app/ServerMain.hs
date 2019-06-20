{-  Executable for a server loop that handles attestation protocol requests.

  Author: Adam Petz
  Date:  06/14/2019
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Copland hiding (main')
import MonadCop
import MonadAM
import Interp
import Comm
--import ClientProgArgs (getClientOptions, Client_Options(..))
import ServerProgArgs (getServerOptions, Server_Options(..))
import qualified JsonCopland as JC (jsonToFile)
import qualified Appraise as APP (appraise)
import qualified CryptoImpl as CI (doHashFile)

import qualified Data.Map as M
import Control.Concurrent.STM
--import qualified Control.Concurrent as CC (forkIO, threadDelay)
--import qualified Control.Concurrent.Thread as CCT (forkIO, result, Result)
import qualified GHC.Conc.Sync as GCS (ThreadId)
import Control.Monad.State
import Control.Monad.Reader
import Data.List(union)
import qualified Data.ByteString as B (empty, writeFile)
import qualified System.Directory as SD (removeFile)
--import qualified Network.Socket as NS (Socket, accept)


import Control.Exception
import System.IO.Error hiding (catch)
import Prelude hiding (catch)
import Text.Read(readMaybe)


main :: IO ()
main = do
  serverMain

serverMain :: IO ()
serverMain = do
  opts <- getServerOptions
  start_standalone_interp_server opts


