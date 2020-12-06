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
    config <- getConfiguration
    let serverId = pid config
    lme <- LME.new $ pid config
    printf "%s started\n" serverId
    outChan <- newChan
    inChan <- newChan
    forkIO $ runServer (local_port config) inChan
    forkIO $ runClient (remote_port config) outChan 
    forkIO $ processInputMessages lme inChan outChan
    forever $ runMessageSource lme outChan

            
     
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: LME.Lme -> Chan S.ByteString -> IO ()
runMessageSource lme chan = do
     msgStr <- encodeMessage <$> LME.request lme
     writeChan chan msgStr
     liftIO $ threadDelay 3000000

processInputMessages :: LME.Lme -> Chan S.ByteString -> Chan S.ByteString -> IO ()
processInputMessages lme inChan outChan = forever $ do
    msgStr <- readChan inChan
    let msg = decodeMessage msgStr
    maybe printErr handleMsg msg
      where
        handleMsg m = do
            print m
            newMsg <- LME.processInputMessage lme m
            forM_ newMsg sendMsg
              where 
                sendMsg msg' = writeChan outChan $ encodeMessage msg'
        printErr = print "Corrupted message detected."