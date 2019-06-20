{-  Command-line interface for an attestation server executable.
    Provides options for (TODO: summary of options here)
    See Main module.

  -- Adapted from tutorial here:
        https://github.com/pcapriotti/optparse-applicative

  Author: Adam Petz
  Date:  06/14/2019
-}

module ServerProgArgs where

import Copland

import Options.Applicative
import Data.Monoid ((<>))

getServerOptions = do
  options <- execParser opts
  return options

-- Record type -----------------
data Server_Options = Server_Options
  { server_optSim :: Bool,
    server_optDebug :: Bool,
    server_serverPort :: String} deriving (Show)

opts :: ParserInfo Server_Options
opts = info (popts <**> helper)
  ( fullDesc
  <> progDesc "Options for a server that interprets Copland phrases."
  <> header "Copland interpreter server")
       
popts :: Parser Server_Options
popts = Server_Options <$> simulation <*> debug <*> sPort
  
simulation :: Parser Bool
simulation = switch
   ( long "sim"
  <> short 's'
  <> help "Set to run in simulation-mode" )

debug :: Parser Bool
debug = switch
   ( long "debug"
  <> short 'd'
  <> help "Verbose debugging output" )

sPort :: Parser String
sPort = strOption
  (  long "portString"
  <> short 'r'
  <> metavar "STRING"
  <> value ""
  <> help "Provide a port string to run a server at.  If this option is omitted or left empty, a random available port will be selected." )
