
module SNMPTrapType
( SNMPTrap (..)
) where

import Data.ASN1.Types (ASN1)
import Data.ConfigFile (SectionSpec)
import Data.Default
import Data.Word (Word8)

data SNMPTrap = SNMPTrap { takeSection :: SectionSpec
                         , takeVersion :: String
                         , takeCommunity :: String
                         , takeAgentAddress :: [Word8]
                         , takeEnterpriseId :: [Integer]
                         , takeGenericTrap :: Integer
                         , takeSpecificTrap :: Integer
                         , takeTrapOid :: [Integer]
                         , takeVarBind :: [ASN1]
                         }

instance Default SNMPTrap where
  def = SNMPTrap { takeSection = undefined
                 , takeVersion = undefined
                 , takeCommunity = undefined
                 , takeAgentAddress = undefined
                 , takeEnterpriseId = undefined
                 , takeGenericTrap = undefined
                 , takeSpecificTrap = undefined
                 , takeTrapOid = undefined
                 , takeVarBind = undefined
                 }


