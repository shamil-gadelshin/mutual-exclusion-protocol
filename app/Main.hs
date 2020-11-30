{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
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
 --   print $ S.breakSubstring "####" "{\n    \"id\": \"server2\",\n    \"timestamp\": \"2\",\n    \"msgType\": \"Request\"\n}####"
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
     msgStr <- encodeMessage <$> composeRequestMessage lts serverId
     writeChan chan msgStr
     liftIO $ threadDelay 3000000

composeReplyMessage :: MVar LTS.Lts -> String -> Integer -> IO Message
composeReplyMessage lts pid msgTs = do
    ltsVal <- takeMVar lts
    let updatedLtsVal = LTS.touch ltsVal
    let updatedLtsVal = LTS.update ltsVal $ LTS.create msgTs
    let ts = LTS.peek updatedLtsVal
    putMVar lts updatedLtsVal
    return $ Message pid ts Reply

composeRequestMessage :: MVar LTS.Lts -> String -> IO Message
composeRequestMessage lts pid = do
    ltsVal <- takeMVar lts
    let updatedLtsVal = LTS.touch ltsVal
    let ts = LTS.peek updatedLtsVal
    putMVar lts updatedLtsVal
    return $ Message pid ts Request

processInputMessages :: String -> MVar LTS.Lts -> Chan S.ByteString -> Chan S.ByteString -> IO ()
processInputMessages serverId lts inChan outChan = forever $ do
    msgStr <- readChan inChan
    let msg = decodeMessage msgStr
    case msg of 
        Just m -> do
            print m
            case msgType m of 
                Request   -> writeChan outChan . encodeMessage =<< composeReplyMessage lts serverId (timestamp m)
                _         -> return () 
        Nothing -> print "Corrupted message detected." -- TODO: Implement message separation
   