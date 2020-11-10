
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8

import Message;
import RedisManager;
import Config

main :: IO ()
main = do
    Char8.putStrLn getMessage
    print =<< getConfiguration
    forkIO $ forever $ do getRedisInfo
    forever $ do updateRedis
    

