
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.Trans

import qualified Control.Exception as E
import qualified Data.ByteString as S
import Control.Concurrent.Chan

import Message;
import RedisManager;
import Config
import TcpManager;
import LTS;

-- Add TCP echo client-server:
    -- catch 'not connected exception'
    -- add composed message
-- Handle exceptions.
-- add config from json for remote servers
-- add distributed process transport
-- add file as protected resource


main :: IO ()
main = do
    config <- getConfiguration
    chan <- newChan
--    forkIO $ case pid config of
    case pid config of
                "server1" -> runServer $ local_port config
                otherwise -> do
                    forkIO $ forever $ runMessageSource (pid config) chan
                    runClient (remote_port config) chan 

                -- $ convertMessage <$> (composeMessage $ )
    
--     forever getRedisInfo
--    forever updateRedis

runMessageSource :: String -> Chan S.ByteString -> IO ()
runMessageSource serverId chan = do
     msgStr <- convertMessage <$> (composeMessage $ serverId)
     writeChan chan msgStr
     liftIO $ threadDelay 1000000

composeMessage :: String -> IO Message
composeMessage pid = do
    ts <- getTimestamp 
    return $ Message pid (show ts) Request

getTimestamp :: IO Integer
getTimestamp = round . (* 1000) <$> getPOSIXTime
