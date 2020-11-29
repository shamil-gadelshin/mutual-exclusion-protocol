
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.ByteString.Char8 as Char8
import Text.Printf

import qualified Data.Text.IO as T
import qualified Control.Exception as E
import qualified Data.ByteString as S


import Message;
import RedisManager;
import Config
import TcpManager;
import qualified LTS;

-- Add TCP echo client-server:
    -- catch 'not connected exception'
    -- reconnect on server shutdown
    
-- Handle exceptions.
-- add config from json for remote servers
-- add distributed process transport
-- add file as protected resource

-- Done:
    -- add composed message


main :: IO ()
main = do
    let ltsVal = LTS.new
    lts <- newMVar ltsVal
    config <- getConfiguration
    printf "%s started\n" (pid config)
    liftIO $ threadDelay 4000000
    chan <- newChan
    forkIO $ runServer $ local_port config
    liftIO $ threadDelay 3000000
    forkIO $ runClient (remote_port config) chan 
    forever $ runMessageSource (pid config) lts chan
    
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: String -> MVar LTS.Lts -> Chan S.ByteString -> IO ()
runMessageSource serverId lts chan = do
     msgStr <- convertMessage <$> (composeMessage lts serverId)
     writeChan chan msgStr
     liftIO $ threadDelay 1000000

composeMessage :: MVar LTS.Lts -> String -> IO Message
composeMessage lts pid = do
    ltsVal <- takeMVar lts
    let updatedLtsVal = LTS.touch ltsVal
    let ts = LTS.peek updatedLtsVal
    putMVar lts updatedLtsVal
    return $ Message pid (show ts) Request
