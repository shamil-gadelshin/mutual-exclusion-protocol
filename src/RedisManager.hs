
{-# LANGUAGE OverloadedStrings #-}

module RedisManager
    ( updateRedis
    , getRedisInfo
    ) where

import Database.Redis
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.ByteString.Char8
import Data.ByteString.Conversion
import Data.Maybe
import Data.Either.Combinators

-- Test redis resource (update).
updateRedis :: IO ()
updateRedis = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        counter <- get counterName
        convCounter <- return . convertCounterValue $ (join . rightToMaybe) counter
        let next = toByteString' . (+1) <$> convCounter
        set counterName $ fromMaybe "0" next
        liftIO $ threadDelay 1000000
        final <- get counterName
        liftIO $ print final

-- Test redis resource (show)
getRedisInfo :: IO ()
getRedisInfo = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        counter <- get counterName
        liftIO $ print counter
        liftIO $ threadDelay 300000

-- Get protected redis resource name
counterName :: ByteString
counterName = "xcounter"

-- Helper function: converts a string to an integer.
convertCounterValue :: Maybe ByteString -> Maybe Integer
convertCounterValue = (<$>) $ read . unpack 

