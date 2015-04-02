
module SendTrap
( sendTrap
, pendingFin
) where

import SNMPTrapType

import Control.Concurrent
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as B

sendTrap :: MVar Int -> Int -> Int -> SockAddr -> (SNMPTrap, B.ByteString) -> IO ()
sendTrap fin sent intval server (trap, msg) = do
  withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
    sendAllTo sock msg server
    close sock
  readMVar fin >>= \f -> case f < 0 of
    True -> do
      threadDelay intval
      sendTrap fin (sent+1) intval server (trap, msg)
    False -> do
      putStrLn $ show sent ++ " traps were successfully sent by [" ++ takeSection trap ++ "]."
      modifyMVar_ fin $ \n -> return (n+1)

pendingFin :: MVar Int -> Int -> IO ()
pendingFin fin threads = readMVar fin >>= \f -> case f < threads of
  True -> do
    threadDelay 10000
    pendingFin fin threads
  False -> return ()

