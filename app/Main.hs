{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Foldable
import Text.Printf   

import qualified Data.Text.IO as T
import qualified Data.ByteString as S

import Message;
import MessageBroker;
import RedisManager;
import Config
import TcpManager;
import qualified LTS;
import qualified LME;

-- Extra:
    -- add config from json for remote servers
    -- add distributed process transport
    -- add file as protected resource
    -- add formatting
    -- add linting

-- Done:
    -- add composed message
    -- catch 'not connected exception' for tcp client
    -- add lamport timestamps

main :: IO ()
main = do
    config <- loadConfigurationFromFile
    let serverId = pid $ local config
    let local_port = port $ local config
    let remote_port = port $ head $ remotes config
    lme <- LME.new serverId
    printf "%s started\n" serverId
    outChan <- newChan
    inChan <- newChan
    let broker = MessageBroker inChan [outChan]
    forkIO $ runServer local_port inChan
    forkIO $ runClient remote_port outChan 
    forkIO $ processInputMessages lme broker
    forever $ runMessageSource lme broker

            
     
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: (Broker br) => LME.Lme -> br -> IO ()
runMessageSource lme broker = do
     reqMsg <- LME.request lme
     broadcast broker reqMsg
     liftIO $ threadDelay 3000000

processInputMessages :: (Broker br) => LME.Lme -> br -> IO ()
processInputMessages lme br = forever $ do
    msg <- receive br
    maybe printErr handleMsg msg
      where
        handleMsg m = do
            print m
            newMsg <- LME.processInputMessage lme m
            forM_ newMsg sendMsg
              where 
                sendMsg msg' = send br "" msg'
        printErr = print "Corrupted message detected."

