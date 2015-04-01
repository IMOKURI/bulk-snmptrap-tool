
module SendTrap
( sendTrap
, pendingFin
) where

import Control.Concurrent
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.ConfigFile (SectionSpec)

sendTrap :: MVar Int -> Int -> Int -> SockAddr -> (SectionSpec, B.ByteString) -> IO ()
sendTrap fin sent intval server (sec, msg) = do
  withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
    sendAllTo sock msg server
    close sock
  readMVar fin >>= \f -> case f < 0 of
    True -> do
      threadDelay intval
      sendTrap fin (sent+1) intval server (sec, msg)
    False -> do
      putStrLn $ show sent ++ " traps were successfully sent by [" ++ sec ++ "]."
      modifyMVar_ fin $ \n -> return (n+1)

pendingFin :: MVar Int -> Int -> IO ()
pendingFin fin threads = readMVar fin >>= \f -> case f < threads of
  True -> do
    threadDelay 10000
    pendingFin fin threads
  False -> return ()

