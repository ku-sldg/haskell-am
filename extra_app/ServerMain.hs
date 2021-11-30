{-  Executable for a server loop that handles attestation protocol requests.

  Author: Adam Petz
  Date:  06/14/2019
-}

module Main where

import ServerAppUtil (start_standalone_server)

import ServerProgArgs (getServerOptions)

main :: IO ()
main = do
  serverMain

serverMain :: IO ()
serverMain = do
  opts <- getServerOptions
  start_standalone_server opts


