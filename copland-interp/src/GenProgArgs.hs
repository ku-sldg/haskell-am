{-  Command-line interface for a datatype/JSON generator executable.
    Provides options for (TODO: summary of options here)
    See GenMain module.

  -- Adapted from tutorial here:
        https://github.com/pcapriotti/optparse-applicative

  Author: Adam Petz
  Date:  06/14/2019
-}

module GenProgArgs where

import Copland

import Options.Applicative
import Data.Monoid ((<>))

getGenOptions = do
  options <- execParser opts
  return options

reqStr = "-q (RequestMessages)"
respStr = "-p (ResponseMessages)"
tStr = "-t (Copland terms)"
evStr = "-e (Evidence terms)"

numTrue :: [Bool] -> Int
numTrue bs = foldl numTrue' 0 bs

  where numTrue' :: Int -> Bool -> Int
        numTrue' i b = if b then i + 1 else i

genOptsConsistent :: Gen_Options -> IO ()
genOptsConsistent (Gen_Options n reqB respB termB evB inFile outFile jsonB) = do

  let numTrues = numTrue [reqB,respB,termB,evB]
  case numTrues of
   1 -> return ()
   _ -> error $ "Command Line Error: please provide EXACTLY ONE of the following options:  " ++ reqStr ++ ", " ++ respStr ++ ", " ++ tStr ++ ", " ++ evStr

  if ((n /= 0) && (inFile /= "")) then error "Command Line Error: cannot both generate inputs(-n) AND provide an input file(-i)" else return ()
  return ()                 

-- Record type -----------------
data Gen_Options = Gen_Options
  { optNumTerms :: Int,
    optReq :: Bool,
    optResp :: Bool,
    optTerm :: Bool,
    optEv :: Bool,   
    optInFile :: FilePath,
    optOutFile :: FilePath, 
    optJson :: Bool   
  } deriving (Show)

opts :: ParserInfo Gen_Options
opts = info (popts <**> helper)
  ( fullDesc
  <> progDesc "Options for Generating Copland json test data"
  <> header "Generating Copland test data" )
       
popts :: Parser Gen_Options
popts = Gen_Options <$> numRand <*> reqBool <*> respBool <*> termBool <*> evBool <*> inFile <*> outFile <*> json

numRand :: Parser Int
numRand = option auto
           ( long "num"
             <> short 'n'
             <> help "Generate N random things(type of thing determined by other options provided)"
             {-<> showDefault-}
             <> value 0
             <> metavar "N" )

reqBool :: Parser Bool
reqBool = switch
   ( long "req"
  <> short 'q'
  <> help "Generate RequestMessages" )

respBool :: Parser Bool
respBool = switch
   ( long "resp"
  <> short 'p'
  <> help "Generate ResponseMessages" )

termBool :: Parser Bool
termBool = switch
   ( long "term"
  <> short 't'
  <> help "Generate Copland Terms" )

evBool :: Parser Bool
evBool = switch
   ( long "ev"
  <> short 'e'
  <> help "Generate Copland Evidence" )
   
inFile :: Parser FilePath
inFile = strOption
  (  long "in"
  <> short 'i'
  <> metavar "FILENAME"
  <> value ""
  <> help "Specify FILENAME as input file (newline-separated, stdIn if omitted)" )
  
outFile :: Parser FilePath
outFile = strOption
  (  long "out"
  <> short 'o'
  <> metavar "FILENAME"
  <> value ""
  <> help "Specify FILENAME as output file (newline-separated, stdOut if omitted)" )

json :: Parser Bool
json = switch
   ( long "data"
  <> short 'd'
  {-<> showDefault-}
  <> help "if set: output datatypes(input JSON), if unset: output JSON(input datatypes)." )
