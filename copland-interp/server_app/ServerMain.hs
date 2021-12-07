{-  Executable for a server loop that handles attestation protocol requests.

  Author: Adam Petz
  Date:  06/14/2019
-}

module Main where

--import ServerAppUtil (start_standalone_server)

import ServerProgArgs (getServerOptions)
import ServerAppUtil(start_server)

main :: IO ()
main = do
  serverMain'

serverMain' :: IO ()
serverMain' = do
  opts' <- getServerOptions
  start_server opts'
  --return ()


