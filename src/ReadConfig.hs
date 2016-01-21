
module ReadConfig
( CommandLineOptions(..)
, commandLineOptions
, readConfig
) where

import SNMPTrapType

import Options.Applicative
import Control.Exception
import Control.Monad.Except
import Data.ASN1.Types
import Data.ConfigFile
import Data.Default
import Data.Either (lefts, rights)
import Data.Either.Utils (forceEither)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Word (Word8)
import Network.Socket (HostName, ServiceName)
import System.Exit (exitFailure)

data CommandLineOptions = CommandLineOptions { takeConfigFile :: FilePath
                                             , takeServerHost :: HostName
                                             , takeServerPort :: ServiceName
                                             , takeSendInterval :: Int
                                             , takeTimer :: Int }

commandLineOptions :: ParserInfo CommandLineOptions
commandLineOptions = info ( helper <*> commandLineOptions')
                       ( fullDesc
                       <> header "bulk-snmptrap-tool - Tool that send many SNMP traps for load test of SNMP manager"
                       <> progDesc "You can use multi cores by adding with '+RTS -N' option to improve performance."
                       <> footer "If this program throw the StackOverflow Exception and you have huge memory, you may avoid this exception by adding with '+RTS -K???M' option. (default: 8M)" )

commandLineOptions' :: Parser CommandLineOptions
commandLineOptions' = CommandLineOptions
  <$> strOption   ( long "config" <> metavar "CONFIGFILE"         <> help "CONFIGFILE that is used for sending SNMP traps" )
  <*> strOption   ( long "host"   <> metavar "HOSTNAME"           <> help "HOSTNAME that is sent SNMP traps" )
  <*> strOption   ( long "port"   <> metavar "PORT"               <> help "PORT that is sent SNMP traps"                <> value "162"   <> showDefault )
  <*> option auto ( long "intval" <> metavar "INTERVAL(microsec)" <> help "Transmission INTERVAL (microsecond (10^-6))" <> value 1000000 <> showDefault )
  <*> option auto ( long "timer"  <> metavar "TIMER(sec)"         <> help "Transmission TIMER (second)"                 <> value 10      <> showDefault )


readConfig :: CommandLineOptions -> IO [SNMPTrap]
readConfig opts = do
  cp <- forceEither <$> readfile emptyCP (takeConfigFile opts)
  when (null (sections cp)) (throwIO $ ErrorCall "Section does not exist.")

  let snmptraps = makeSNMPTrap cp (sections cp)
  if null $ lefts snmptraps then return $ rights snmptraps else ((print $ head $ lefts snmptraps) >> exitFailure)


makeSNMPTrap :: ConfigParser -> [SectionSpec] -> [Either CPError SNMPTrap]
makeSNMPTrap _ [] = []
makeSNMPTrap cp (s:ss) = case simpleAccess cp s "snmp_version" of
  Right "1"  -> makeSNMPTrap1 cp s: makeSNMPTrap cp ss
  Right "2c" -> makeSNMPTrap2 cp s: makeSNMPTrap cp ss
  Right _    -> Left (ParseError "snmp_version", "snmp_version of [" ++ s ++ "] is invalid."): makeSNMPTrap cp ss
  Left err   -> Left err: makeSNMPTrap cp ss


makeSNMPTrap1 :: ConfigParser -> SectionSpec -> Either CPError SNMPTrap
makeSNMPTrap1 cp1 s1 = do
  comm       <- get cp1 s1 "snmp_community" `catchError` (return . const "public")
  agent      <- get cp1 s1 "agent_ip_address" `catchError` (return . const "127.0.0.1") >>= \agent' ->
                case length $ concatMap (\s -> reads s :: [(Word8,String)]) $ splitOn "." agent' of
                4 -> return $ mapMaybe (fmap fst . listToMaybe . reads) $ splitOn "." agent'
                _ -> Left (ParseError "agent_ip_address", "agent_ip_address of [" ++ s1 ++ "] is invalid.")
  enterprise <- get cp1 s1 "enterprise_oid" >>= \enterprise' ->
                if null $ concatMap (\s -> reads s :: [(Integer,String)]) $ dropWhile (=="") $ splitOn "." enterprise'
                then Left (ParseError "enterprise_oid", "enterprise_oid of [" ++ s1 ++ "] is invalid.")
                else return $ mapMaybe (fmap fst . listToMaybe . reads) $ dropWhile (=="") $ splitOn "." enterprise'
  generic    <- get cp1 s1 "generic_trap"
  specific   <- get cp1 s1 "specific_trap"
  varbind    <- get cp1 s1 "varbind" `catchError` (return . const "") >>=
                makeVarBind . fmap words . filter (/="") . splitOneOf "\n"

  return def { takeSection = s1
             , takeVersion = "1"
             , takeCommunity = comm
             , takeAgentAddress = agent
             , takeEnterpriseId = enterprise
             , takeGenericTrap = generic
             , takeSpecificTrap = specific
             , takeVarBind = varbind }


makeSNMPTrap2 :: ConfigParser -> SectionSpec -> Either CPError SNMPTrap
makeSNMPTrap2 cp2 s2 = do
  comm    <- get cp2 s2 "snmp_community" `catchError` (return . const "public")
  trapoid <- get cp2 s2 "snmptrap_oid" >>= \trapoid' ->
             if null $ concatMap (\s -> reads s :: [(Integer,String)]) $ dropWhile (=="") $ splitOn "." trapoid'
             then Left (ParseError "snmptrap_oid", "snmptrap_oid of [" ++ s2 ++ "] is invalid.")
             else return $ mapMaybe (fmap fst . listToMaybe . reads) $ dropWhile (=="") $ splitOn "." trapoid'
  varbind <- get cp2 s2 "varbind" `catchError` (return . const "") >>=
             makeVarBind . fmap words . filter (/="") . splitOneOf "\n"

  return def { takeSection = s2
             , takeVersion = "2c"
             , takeCommunity = comm
             , takeTrapOid = trapoid
             , takeVarBind = varbind }


makeVarBind :: [[String]] -> Either CPError [ASN1]
makeVarBind [] = Right []
makeVarBind (s:ss) = if null $ concatMap (\s' -> reads s' :: [(Integer,String)]) $ dropWhile (=="") $ splitOn "." $ head s
                     then Left (ParseError "varbind", "Oid of '" ++ unwords s ++ "' is invalid.")
                     else case s !! 1 of
                          "s" -> (++) <$> Right [Start Sequence, oid s, msgstr s, End Sequence] <*> makeVarBind ss
                          "i" -> if null $ (\i -> reads i :: [(Integer,String)]) $ concat $ drop 2 s
                                 then Left (ParseError "varbind", "Value of '" ++ unwords s ++ "' is invalid.")
                                 else (++) <$> Right [Start Sequence, oid s, msgint s, End Sequence] <*> makeVarBind ss
                          _ -> Left (ParseError "varbind", "Type of '" ++ unwords s ++ "' is invalid.")
  where oid so = OID (mapMaybe (fmap fst . listToMaybe . reads) $ dropWhile (=="") $ splitOn "." $ head so)
        msgstr sstr = OctetString (encodeUtf8 $ T.pack $ unwords $ drop 2 sstr)
        msgint sint = IntVal (read $ concat $ drop 2 sint)


