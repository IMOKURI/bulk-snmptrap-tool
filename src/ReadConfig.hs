
module ReadConfig
( CommandLineOptions(..)
, commandLineOptions
, readConfig
) where

import Options.Applicative
import Control.Exception
import Control.Monad.Error
import Data.ConfigFile
import Data.Maybe (isJust, fromJust)
import Network.Socket (HostName, ServiceName)
import System.Directory (doesFileExist)

data CommandLineOptions = CommandLineOptions { takeConfigFile :: FilePath
                                             , takeServerIp :: HostName
                                             , takeServerPort :: ServiceName
                                             , takeSendInterval :: Int
                                             , takeTimer :: Int }

commandLineOptions :: ParserInfo CommandLineOptions
commandLineOptions = info ( helper <*> commandLineOptions')
                       ( fullDesc
                       <> header "snmptrapper - Tool that send many SNMP traps for load test of SNMP manager"
                       <> progDesc "You can use multi cores by adding with '+RTS -N' option to improve performance."
                       <> footer "If this program throw the StackOverflow Exception and you have huge memory, you may avoid this exception by adding with '+RTS -K???M' option. (default: 8M)" )

commandLineOptions' :: Parser CommandLineOptions
commandLineOptions' = CommandLineOptions
  <$> strOption   ( long "config" <> metavar "CONFIGFILE" <> help "CONFIGFILE that is used for sending SNMP traps" )
  <*> strOption   ( long "host"   <> metavar "HOSTNAME"   <> help "HOSTNAME that is sent SNMP traps" )
  <*> strOption   ( long "port"   <> metavar "PORT"       <> help "PORT that is sent SNMP traps"                <> value "162"   <> showDefault )
  <*> option auto ( long "intval" <> metavar "INTERVAL"   <> help "Transmission INTERVAL (microsecond (10^-6))" <> value 1000000 <> showDefault )
  <*> option auto ( long "timer"  <> metavar "TIMER"      <> help "Transmission TIMER (second)"                 <> value 10      <> showDefault )


readConfig :: CommandLineOptions -> IO (Either CPError ConfigParser)
readConfig opts = runErrorT $ do
  file <- liftIO $ do
    existence <- doesFileExist (takeConfigFile opts)
    if existence then return (takeConfigFile opts) else throwIO (ErrorCall "Configuration file does not exist.")

  cp <- join $ liftIO $ readfile emptyCP file

  let chkTrapOps = checkTrapOptions cp (sections cp)

  liftIO $ when (null (sections cp)) (throwIO $ ErrorCall "Section does not exist.")
  liftIO $ when (isJust chkTrapOps) (throwIO $ ErrorCall $ show (fromJust chkTrapOps) ++ " is invalid or does not exist." )

  return cp


checkTrapOptions :: ConfigParser -> [SectionSpec] -> Maybe (SectionSpec, OptionSpec)
checkTrapOptions _ [] = Nothing
checkTrapOptions cp (s:ss) | simpleAccess cp s "snmp_version" == Right "1" = checkTrapOption cp (s:ss) optionsList1
                           | simpleAccess cp s "snmp_version" == Right "2c" = checkTrapOption cp (s:ss) optionsList2c
                           | otherwise = Just (s, "snmp_version")
  where checkTrapOption cp' (_:ss') [] = checkTrapOptions cp' ss'
        checkTrapOption _ [] _ = Nothing
        checkTrapOption cp' (s':ss') (o:os) | has_option cp' s' o = checkTrapOption cp' (s':ss') os
                                            | otherwise           = Just (s', o)

optionsList1 :: [OptionSpec]
optionsList1 = ["enterprise_oid", "generic_trap", "specific_trap"]

optionsList2c :: [OptionSpec]
optionsList2c = ["snmptrap_oid"]

