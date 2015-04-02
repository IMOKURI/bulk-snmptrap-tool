
module Main where

import ReadConfig
import ASN1Trap
import SendTrap

import Control.Concurrent
import Control.Parallel.Strategies
import Network.Socket
import Options.Applicative


main :: IO ()
main = do
  opts <- execParser commandLineOptions
  sockAddr <- addrAddress <$> head <$> getAddrInfo Nothing (Just (takeServerIp opts)) (Just (takeServerPort opts))

  snmpTraps <- readConfig opts
  let trapMsgs = zip snmpTraps $ makeASN1TrapMsgs snmpTraps

-- Flag of stopped process.
--  -1: Send trap in progress.
--   0: Start stopping threads.
--   n: Number of stopped thread.
  fin <- newMVar (-1)

  mapM_ forkIO $ parMap rseq (sendTrap fin 1 (takeSendInterval opts) sockAddr) trapMsgs

  threadDelay (takeTimer opts * 1000000) >> modifyMVar_ fin (const $ return 0)

  pendingFin fin (length trapMsgs)


