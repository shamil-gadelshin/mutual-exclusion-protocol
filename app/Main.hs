
{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.ByteString.Char8
import Data.ByteString.Conversion
import Data.Maybe


main :: IO ()
main = do
    forkIO $ forever $ do getRedisInfo
    forever $ do updateRedis
    

updateRedis :: IO ()
updateRedis = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        zcounter <- get "zcounter"
        convzcounter <- case zcounter of Right (Just zcounterStr) -> return $ Just (read . unpack $ zcounterStr :: Integer)
                                         _ -> return Nothing
        next <- return $ toByteString' <$> (+1) <$> convzcounter
        set "zcounter" $ fromMaybe "11" next

        liftIO $ threadDelay 1000000
        final <- get "zcounter"
        liftIO $ print $ final

getRedisInfo :: IO ()
getRedisInfo = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        zcounter <- get "zcounter"
        liftIO $ print $ zcounter
        liftIO $ threadDelay 300000