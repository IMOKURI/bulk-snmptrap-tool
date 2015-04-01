
module ASN1Trap
( makeASN1TrapMsgs
) where

import GHC.Word (Word8)
import Data.ASN1.Types
import Data.ASN1.Encoding (encodeASN1)
import Data.ASN1.BinaryEncoding
import Data.Serialize (encode)
import Data.ConfigFile
import Data.List.Split (splitOn,splitOneOf)
import Data.Either.Utils (forceEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding

makeASN1TrapMsgs :: ConfigParser -> [SectionSpec] -> [B.ByteString]
makeASN1TrapMsgs _ [] = []
makeASN1TrapMsgs cp (s:ss) = (B.concat $ BL.toChunks $ encodeASN1 DER (makeASN1TrapData cp s)) : makeASN1TrapMsgs cp ss


makeASN1TrapData :: ConfigParser -> SectionSpec -> [ASN1]
makeASN1TrapData cp sec | version == "1" = asn1Trap1Data cp sec
                        | version == "2c" = asn1Trap2Data cp sec
                        | otherwise = [Null]
  where version = either (const "") id $ get cp sec "snmp_version"


asn1Trap1Data :: ConfigParser -> SectionSpec -> [ASN1]
asn1Trap1Data cp sec = [ Start Sequence                   -- SNMP packet start
                       , IntVal 0                           -- SNMP version: version-1
                       , OctetString community              -- SNMP community
                       , Start (Container Context 4)        -- SNMP trap pdu v1 start
                       , OID enterpriseId                     -- Enterprise OID
                       , Other Application 0 agentAddress     -- Agent Address
                       , IntVal genericTrap                   -- Generic trap
                       , IntVal specificTrap                  -- Specific trap
                       , Other Application 3 timeTicks        -- Time ticks
                       , Start Sequence                       -- Variable binding list start
                       ] ++ varbind1 ++
                       [ End Sequence                         -- Variable binding list end
                       , End (Container Context 4)          -- SNMP trap pdu v1 end
                       , End Sequence                     -- SNMP Packaet end
                       ]
  where community = C.pack $ either (const "public") id $ get cp sec "snmp_community"
        enterpriseId = map (\s -> read s :: Integer) $ dropWhile (=="") $ splitOn "." $ forceEither $ get cp sec "enterprise_oid"
        agentAddress = B.pack $ map (\s -> read s :: Word8) $ splitOn "." $ either (const "127.0.0.1") id $ get cp sec "agent_ip_address"
        genericTrap = forceEither $ get cp sec "generic_trap" :: Integer
        specificTrap = forceEither $ get cp sec "specific_trap" :: Integer
        timeTicks = B.dropWhile (==0) $ encode (12345 :: Integer) -- This should be sysUpTime. But This tool use fixed number for high performance.
        varbind1 = varBindData $ filter (/="") $ splitOneOf "\n" $ either (const "") id $ get cp sec "varbind"


asn1Trap2Data :: ConfigParser -> SectionSpec -> [ASN1]
asn1Trap2Data cp sec = [ Start Sequence                   -- SNMP packet start
                       , IntVal 1                           -- SNMP version: version-2c
                       , OctetString community              -- SNMP community
                       , Start (Container Context 7)        -- SNMP trap pdu v2c start
                       , IntVal requestId                     -- Request-id
                       , IntVal 0                             -- Error Status: noError
                       , IntVal 0                             -- Error Index
                       , Start Sequence                       -- Variable binding list start
                       , Start Sequence                         -- 1st variable binding start
                       , OID timeTicksOid                         -- Object name: sysUpTimeInstance
                       , Other Application 3 timeTicks            -- Time ticks
                       , End Sequence                           -- 1st variable binding end
                       , Start Sequence                         -- 2nd variable binding start
                       , OID snmpTrapOid                          -- Object name: snmpTrapOID
                       , OID snmpTrapOidValue                     -- SNMP Trap OID
                       , End Sequence                           -- 2nd variable binding end
                       ] ++ varbind2 ++
                       [ End Sequence                         -- Variable binding list end
                       , End (Container Context 4)          -- SNMP trap pdu v2c end
                       , End Sequence                     -- SNMP Packaet end
                       ]
  where community = C.pack $ either (const "public") id $ get cp sec "snmp_community"
        requestId = 12345 -- This should be random number. But This tool use fixed number for high performance.
        timeTicksOid = [1,3,6,1,2,1,1,3,0]
        timeTicks = B.dropWhile (==0) $ encode (12345 :: Integer) -- This should be sysUpTime. But This tool use fixed number for high performance.
        snmpTrapOid = [1,3,6,1,6,3,1,1,4,1,0]
        snmpTrapOidValue = map (\s -> read s :: Integer) $ dropWhile (=="") $ splitOn "." $ forceEither $ get cp sec "snmptrap_oid"
        varbind2 = varBindData $ filter (/="") $ splitOneOf "\n" $ either (const "") id $ get cp sec "varbind"


varBindData :: [String] -> [ASN1]
varBindData [] = []
varBindData (s:ss) = Start Sequence: oid: msg: End Sequence: varBindData ss
  where o:t:m = words s
        oid = OID (map (\s' -> read s' :: Integer) $ dropWhile (=="") $ splitOn "." o)
        msg = case t of
          "i" -> IntVal (read $ concat m)
          _   -> OctetString (encodeUtf8 $ T.pack $ unwords m)

