
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-cse #-}

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as Char8
import System.Console.CmdArgs

import Message;
import RedisManager;

data Sample = Sample {hello :: String}
              deriving (Show, Data, Typeable)

sample = Sample{hello = def}

main :: IO ()
main = do
    Char8.putStrLn getMessage
    print =<< cmdArgs sample
    forkIO $ forever $ do getRedisInfo
    forever $ do updateRedis
    

