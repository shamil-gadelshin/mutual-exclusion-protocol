
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Control.Exception as E
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Message;
import RedisManager;
import Config
import TcpManager;

-- Add TCP echo client-server:
    -- catch 'not connected exception'
    -- add composed message
-- Handle exceptions.
-- add config from json for remote servers
-- add distributed process transport


main :: IO ()
main = do
    config <- getConfiguration
    
    

--    Char8.putStrLn =<< convertMessage <$> (composeMessage $ pid config)
    Char8.putStrLn =<< convertMessage <$> (composeMessage $ pid config)
    
    forkIO $ case pid config of
                "server1" -> runServer $ local_port config
                otherwise -> runClient (remote_port config) (\() -> convertMessage <$> (composeMessage $ pid config))
    forkIO $ forever getRedisInfo
    forever updateRedis

composeMessage :: String -> IO Message
composeMessage pid = do
    ts <- getTimestamp 
    return $ Message pid (show ts)

getTimestamp :: IO Integer
getTimestamp = round . (* 1000) <$> getPOSIXTime
