
module ASN1Trap
( makeASN1TrapMsgs
) where

import SNMPTrapType

import Data.ASN1.Types
import Data.ASN1.Encoding (encodeASN1)
import Data.ASN1.BinaryEncoding
import Data.Serialize (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C

makeASN1TrapMsgs :: [SNMPTrap] -> [B.ByteString]
makeASN1TrapMsgs [] = []
makeASN1TrapMsgs (s:ss) = (B.concat $ BL.toChunks $ encodeASN1 DER (makeASN1TrapData s)) : makeASN1TrapMsgs ss


makeASN1TrapData :: SNMPTrap -> [ASN1]
makeASN1TrapData s = case takeVersion s of
  "1" -> asn1Trap1Data s
  "2c" -> asn1Trap2Data s
  _ -> [Null]


asn1Trap1Data :: SNMPTrap -> [ASN1]
asn1Trap1Data s = [ Start Sequence                   -- SNMP packet start
                  , IntVal 0                           -- SNMP version: version-1
                  , OctetString (C.pack $ takeCommunity s)              -- SNMP community
                  , Start (Container Context 4)        -- SNMP trap pdu v1 start
                  , OID (takeEnterpriseId s)                     -- Enterprise OID
                  , Other Application 0 (B.pack $ takeAgentAddress s)     -- Agent Address
                  , IntVal (takeGenericTrap s)                   -- Generic trap
                  , IntVal (takeSpecificTrap s)                  -- Specific trap
                  , Other Application 3 timeTicks        -- Time ticks
                  , Start Sequence                       -- Variable binding list start
                  ] ++ (takeVarBind s) ++
                  [ End Sequence                         -- Variable binding list end
                  , End (Container Context 4)          -- SNMP trap pdu v1 end
                  , End Sequence                     -- SNMP Packaet end
                  ]
  where timeTicks = B.dropWhile (==0) $ encode (12345 :: Integer) -- This should be sysUpTime. But This tool use fixed number for high performance.


asn1Trap2Data :: SNMPTrap -> [ASN1]
asn1Trap2Data s = [ Start Sequence                   -- SNMP packet start
                  , IntVal 1                           -- SNMP version: version-2c
                  , OctetString (C.pack $ takeCommunity s)              -- SNMP community
                  , Start (Container Context 7)        -- SNMP trap pdu v2c start
                  , IntVal requestId                     -- Request-id
                  , IntVal 0                             -- Error Status: noError
                  , IntVal 0                             -- Error Index
                  , Start Sequence                       -- Variable binding list start
                  , Start Sequence                         -- 1st variable binding start
                  , OID [1,3,6,1,2,1,1,3,0]                  -- Object name: sysUpTimeInstance
                  , Other Application 3 timeTicks            -- Time ticks
                  , End Sequence                           -- 1st variable binding end
                  , Start Sequence                         -- 2nd variable binding start
                  , OID [1,3,6,1,6,3,1,1,4,1,0]              -- Object name: snmpTrapOID
                  , OID (takeTrapOid s)                          -- SNMP Trap OID
                  , End Sequence                           -- 2nd variable binding end
                  ] ++ (takeVarBind s) ++
                  [ End Sequence                         -- Variable binding list end
                  , End (Container Context 7)          -- SNMP trap pdu v2c end
                  , End Sequence                     -- SNMP Packaet end
                  ]
  where requestId = 12345 -- This should be random number. But This tool use fixed number for high performance.
        timeTicks = B.dropWhile (==0) $ encode (12345 :: Integer) -- This should be sysUpTime. But This tool use fixed number for high performance.


