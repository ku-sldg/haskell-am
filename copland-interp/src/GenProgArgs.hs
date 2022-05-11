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

getGenOptions = do
  options <- execParser opts
  return options

reqStr = "-q (RequestMessages)"
respStr = "-p (ResponseMessages)"
tStr = "-t (Copland terms)"
evStr = "-e (Concrete Evidence terms)"
evtStr = "-e (Evidence Type terms)"

numTrue :: [Bool] -> Int
numTrue bs = foldl numTrue' 0 bs

  where numTrue' :: Int -> Bool -> Int
        numTrue' i b = if b then i + 1 else i

genOptsConsistent :: Gen_Options -> IO ()
genOptsConsistent (Gen_Options n reqB respB termB evB evtB sigReqB sigRespB aspReqB aspRespB inFile outFile jsonB localB) = do
  
  let numTrues = numTrue [reqB,respB,termB,evB,evtB,sigReqB,sigRespB,aspReqB,aspRespB]
  case numTrues of
   1 -> return ()
   _ -> error $ "Command Line Error: please provide EXACTLY ONE of the following options:  " ++ reqStr ++ ", " ++ respStr ++ ", " ++ tStr ++ ", " ++ evStr ++ evtStr

  if ((n /= 0) && (inFile /= "")) then error "Command Line Error: cannot both generate inputs(-n) AND provide an input file(-i)" else return ()

  if ((localB == True) && (inFile /= "")) then error "Command Line Error: cannot use both local terms(-l) AND provide an input file(-i)" else return ()
  return ()                 

-- Record type -----------------
data Gen_Options = Gen_Options
  { optNumTerms :: Int,
    optReq :: Bool,
    optResp :: Bool,
    optTerm :: Bool,
    optEv :: Bool,
    optEvt :: Bool,
    optSigReq :: Bool,
    optSigResp :: Bool,
    optAspReq :: Bool,
    optAspResp :: Bool,
    optInFile :: FilePath,
    optOutFile :: FilePath, 
    optJson :: Bool,
    optLocal :: Bool
  } deriving (Show)

opts :: ParserInfo Gen_Options
opts = info (popts <**> helper)
  ( fullDesc
  <> progDesc "Options for Generating Copland json test data"
  <> header "Generating Copland test data" )
       
popts :: Parser Gen_Options
popts = Gen_Options <$> numRand <*> reqBool <*> respBool <*> termBool <*> evBool <*> evtBool <*> sigreqBool <*> sigrespBool <*> aspreqBool <*> asprespBool <*> inFile <*> outFile <*> json <*> local

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

sigreqBool :: Parser Bool
sigreqBool = switch
   ( long "sigreq"
  <> short 's'
  <> help "Generate SigRequestMessages" )

sigrespBool :: Parser Bool
sigrespBool = switch
   ( long "sigresp"
  <> short 'u'
  <> help "Generate SigResponseMessages" )

aspreqBool :: Parser Bool
aspreqBool = switch
   ( long "aspreq"
  <> short 'k'
  <> help "Generate AspRequestMessages" )

asprespBool :: Parser Bool
asprespBool = switch
   ( long "aspresp"
  <> short 'm'
  <> help "Generate AspResponseMessages" )

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

evtBool :: Parser Bool
evtBool = switch
   ( long "evt"
  <> short 'y'
  <> help "Generate Copland Evidence Type" )
   
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

local :: Parser Bool
local = switch
   ( long "local"
  <> short 'l'
  {-<> showDefault-}
  <> help "if set: use local list of Haskell-defined datatypes, defined in Main module of generator:  GenMain.hs" )
