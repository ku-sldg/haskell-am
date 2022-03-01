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

getServerOptions = do
  options <- execParser opts
  return options

-- Record type -----------------
data Server_Options = Server_Options
  { server_optSim :: Bool,
    server_optDebug :: Bool,
    server_serverPort :: String,
    server_serverType :: ServerType

    {-,
    server_compile :: Bool-} } deriving (Show)

opts :: ParserInfo Server_Options
opts = info (popts <**> helper)
  ( fullDesc
  <> progDesc "Options for a server that interprets Copland phrases."
  <> header "Copland interpreter server")
       
popts :: Parser Server_Options
popts = Server_Options <$> simulation <*> debug <*> sPort <*> sType

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
  <> help "Provide a port string to run a server at.  If this option is omitted or assigned the empty string, a random available port will be selected." )

sType :: Parser ServerType
sType = sTypeSign <|> sTypeStore <|> sTypeAspServ <|> sTypeCvmServ <|> sTypePar

sTypeSign :: Parser ServerType
sTypeSign = flag' SIGN
  ( long "sign_server"
  <> help "Set for a SIGN ServerType" )

sTypeStore :: Parser ServerType
sTypeStore = flag' STORE
  ( long "store_server"
  <> help "Set for a STORE ServerType" )

sTypePar :: Parser ServerType
sTypePar = flag' STORE
  ( long "par_server"
  <> help "Set for a PAR ServerType" )

sTypeAspServ :: Parser ServerType
sTypeAspServ = ASP_SERV <$> aspid_parser

sTypeCvmServ :: Parser ServerType
sTypeCvmServ = CVM_SERV <$> cvmServParams

{-
sTypeParServ :: Parser ServerType
sTypeParServ = PAR_SERV <$> cvmServParams
-}

cvmServParams :: Parser CVM_SERV_Params
cvmServParams = CVM_SERV_Params <$> plc_parser <*> cvm_asp_spawn <*> cvm_sig_mech_parse

aspid_parser :: Parser ASP_ID
aspid_parser = option auto
  ( long "asp_server"
  <> short 'a' )

plc_parser :: Parser Plc
plc_parser = option auto
  ( long "cvm_plc"
  <> short 'p' )

cvm_sig_port_parse :: Parser Sign_Mechanism
cvm_sig_port_parse = Sign_Server_Addr <$>
  strOption
  ( long "cvm_sig_port"
  <> short 'c' )

cvm_sig_path_parse :: Parser Sign_Mechanism
cvm_sig_path_parse = Sign_Keypath <$>
  strOption
  ( long "cvm_sig_path"
  <> short 'f' )

cvm_asp_spawn :: Parser Bool
cvm_asp_spawn = switch
   ( long "aspspawn"
  <> short 't'
  <> help "Set when asps with auto-generated addresses are running in background" )


cvm_sig_mech_parse :: Parser Sign_Mechanism
cvm_sig_mech_parse = cvm_sig_port_parse <|> cvm_sig_path_parse













