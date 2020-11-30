
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

-- Extra:
    -- add config from json for remote servers
    -- add distributed process transport
    -- add file as protected resource

-- Done:
    -- add composed message
    -- catch 'not connected exception' for tcp client
    -- add lamport timestamps


main :: IO ()
main = do
    let ltsVal = LTS.new
    lts <- newMVar ltsVal
    config <- getConfiguration
    printf "%s started\n" (pid config)
    outChan <- newChan
    inChan <- newChan
    forkIO $ runServer (local_port config) inChan
    forkIO $ runClient (remote_port config) outChan 
    forkIO $ processInputMessages (pid config) lts inChan outChan
    forever $ runMessageSource (pid config) lts outChan
     
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: String -> MVar LTS.Lts -> Chan S.ByteString -> IO ()
runMessageSource serverId lts chan = do
     msgStr <- encodeMessage <$> composeMessage lts serverId
     writeChan chan msgStr
     liftIO $ threadDelay 1000000

composeMessage :: MVar LTS.Lts -> String -> IO Message
composeMessage lts pid = do
    ltsVal <- takeMVar lts
    let updatedLtsVal = LTS.touch ltsVal
    let ts = LTS.peek updatedLtsVal
    putMVar lts updatedLtsVal
    return $ Message pid (show ts) Request

processInputMessages :: String -> MVar LTS.Lts -> Chan S.ByteString -> Chan S.ByteString -> IO ()
processInputMessages serverId lts inChan outChan = forever $ do
    msgStr <- readChan inChan
    let msg = decodeMessage msgStr
    print msg
    -- case msg of 
    --     Just m -> writeChan outChan $ encodeMessage m 
    --     Nothing -> error "Cannot decode input message."
   