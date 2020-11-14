
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (getPOSIXTime)

import Message;
import RedisManager;
import Config

-- Add TCP echo client-server
-- Handle exceptions.

main :: IO ()
main = do
    config <- getConfiguration
    ts <- getTimestamp 
    msg <- return $ Message (pid config) (show ts)
    
    Char8.putStrLn $ convertMessage msg
    print ts
    print config
    
    forkIO $ forever getRedisInfo
    forever updateRedis
    

getTimestamp :: IO Integer 
getTimestamp = round . (* 1000) <$> getPOSIXTime