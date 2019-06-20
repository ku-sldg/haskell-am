{-  Command-line interface for an attestation client(attestation manager + appraiser) executable.
    Provides options for (TODO: summary of options here)
    See AppMain module.

  -- Adapted from tutorial here:
        https://github.com/pcapriotti/optparse-applicative

  Author: Adam Petz
  Date:  06/14/2019
-}

module ClientProgArgs where

import Copland

import Options.Applicative
import Data.Monoid ((<>))

getClientOptions = do
  options <- execParser opts
  return options

-- Record type -----------------
data Client_Options = Client_Options
  { optTermIn :: FilePath,
    optOut :: FilePath,
    optEvIn :: FilePath,
    {-upotIn :: Bool, -}
    optSim :: Bool,
    optProv :: Bool,
    optJson :: Bool,
    optDebug :: Bool,
    optSpawn :: Bool,
    optSpawnSim :: Bool,
    optSpawnDebug :: Bool,
    optNames :: FilePath,
    optApp :: Bool
  } deriving (Show)

opts :: ParserInfo Client_Options
opts = info (popts <**> helper)
  ( fullDesc
  <> progDesc "Options for a client that sends requests to Copland interpreter servers and performs appraisal."
  <> header "Copland interpreter client" )
       
popts :: Parser Client_Options
popts = Client_Options <$> tinput <*> output <*> einput <*> {-uinput <*> -} simulation <*> provision <*> jsonOutput <*> debug <*> spawn <*> spawnSim <*> spawnDebug <*> names <*> appr

tinput :: Parser FilePath
tinput = strOption
  (  long "termfile"
  <> short 't'
  <> metavar "FILENAME"
  <> value ""
  <> help "Read term from Input file FILENAME" )

output :: Parser FilePath
output = strOption
  (  long "outfile"
  <> short 'o'
  <> metavar "FILENAME"
  <> value ""
  <> help "Write to output file FILENAME" )

einput :: Parser FilePath
einput = strOption
  (  long "evfile"
  <> short 'e'
  <> metavar "FILENAME"
  <> value ""
  <> help "Read initial evidence from Input file FILENAME" )
{-
uinput :: Parser Bool
uinput = switch
   ( long "user"
  <> short 'u'
  <> help "Ask user to input a Copland term and initial evidence interactively." )
-}
   
simulation :: Parser Bool
simulation = switch
   ( long "sim"
  <> short 's'
  <> help "Set to run in simulation-mode(simulated local measurement/crypto)" )

provision :: Parser Bool
provision = switch
   ( long "prov"
  <> short 'p'
  <> help "Run provisioning" )

jsonOutput :: Parser Bool
jsonOutput = switch
   ( long "json"
  <> short 'j'
  <> help "Output Json to file" )

debug :: Parser Bool
debug = switch
   ( long "debug"
  <> short 'd'
  <> help "verbose debugging output" )

spawn :: Parser Bool
spawn = switch
   ( long "spawn"
  <> short 'w'
  <> help "Spawn attestation servers as as separate background threads for all places involved in a protocol term" )

spawnSim :: Parser Bool
spawnSim = switch
   ( long "spawnSim"
  <> short 'v'
  <> help "Specify that spawned servers run in simulation mode" )

spawnDebug :: Parser Bool
spawnDebug = switch
   ( long "spawnDebug"
  <> short 'g'
  <> help "Specify that spawned servers run in debug mode" )

appr :: Parser Bool
appr = switch
   ( long "appraise"
  <> short 'a'
  <> help "Perform appraisal on the resulting evidence" )
   
names :: Parser FilePath
names = strOption
  (  long "namesFile"
  <> short 'n'
  <> metavar "FILENAME"
  <> value ""
  <> help "Read mapping from places to names from Input file FILENAME" )
