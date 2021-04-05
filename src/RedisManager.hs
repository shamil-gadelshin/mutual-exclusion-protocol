-- | Defines operation with Redis database.

{-# LANGUAGE OverloadedStrings #-}

module RedisManager
  ( incrementCounter
  , showCounter
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8
import           Data.ByteString.Conversion
import           Data.Either.Combinators
import           Data.Maybe
import           Database.Redis

-- | Increment redis-based counter.
incrementCounter :: ByteString -> IO ()
incrementCounter counterName = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    counter <- get counterName
    let convCounter = convertCounterValue $ (join . rightToMaybe) counter
    let next = toByteString' . (+1) <$> convCounter
    set counterName $ fromMaybe "0" next
    return ()

-- | Show redis-based counter.
showCounter :: ByteString -> IO ()
showCounter counterName = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    counter <- get counterName
    liftIO $ print counter

-- Helper function: converts a string to an integer.
convertCounterValue :: Maybe ByteString -> Maybe Integer
convertCounterValue = (<$>) $ read . unpack

