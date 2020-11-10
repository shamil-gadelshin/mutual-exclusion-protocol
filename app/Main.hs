
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (getPOSIXTime)

import Message;
import RedisManager;
import Config

main :: IO ()
main = do
    print =<< getTimestamp 
    Char8.putStrLn getMessage
    print =<< getConfiguration
    forkIO $ forever $ do getRedisInfo
    forever $ do updateRedis
    

getTimestamp :: IO Integer 
getTimestamp = round . (* 1000) <$> getPOSIXTime