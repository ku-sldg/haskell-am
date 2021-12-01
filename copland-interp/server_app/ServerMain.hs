{-  Executable for a server loop that handles attestation protocol requests.

  Author: Adam Petz
  Date:  06/14/2019
-}

module Main where

--import ServerAppUtil (start_standalone_server)

import ServerProgArgs (getServerOptions)
import ServerAppUtil(start_server)
import DemoStates(sample_server_args)

main :: IO ()
main = do
  serverMain'

serverMain' :: IO ()
serverMain' = do
  --opts <- getServerOptions
  let opts = sample_server_args
  start_server opts
  return ()
  --start_standalone_server opts


