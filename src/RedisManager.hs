
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

updateRedis :: IO ()
updateRedis = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        counter <- get counterName
        convCounter <- case counter of Right (Just counterStr) -> return $ Just (read . unpack $ counterStr :: Integer)
                                       _ -> return Nothing
        next <- return $ toByteString' <$> (+1) <$> convCounter
        set counterName $ fromMaybe "0" next
        liftIO $ threadDelay 1000000
        final <- get counterName
        liftIO $ print $ final

getRedisInfo :: IO ()
getRedisInfo = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        counter <- get counterName
        liftIO $ print $ counter
        liftIO $ threadDelay 300000


counterName :: ByteString
counterName = "xcounter"