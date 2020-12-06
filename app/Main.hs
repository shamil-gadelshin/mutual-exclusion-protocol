{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Text.Printf   
import Data.UUID
import System.Random

import qualified Data.Text.IO as T
import qualified Control.Exception as E
import qualified Data.ByteString as S
import qualified Data.PQueue.Prio.Min as MQ

import Message;
import LME;
import RedisManager;
import Config
import TcpManager;
import qualified LTS;

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

type MessagePriorityQueue = MQ.MinPQueue Integer Message

main :: IO ()
main = do
    let mqVal = MQ.empty
    mq <- newMVar mqVal
    let ltsVal = LTS.new
    lts <- newMVar ltsVal
    config <- getConfiguration
    printf "%s started\n" (pid config)
    outChan <- newChan
    inChan <- newChan
    forkIO $ runServer (local_port config) inChan
    forkIO $ runClient (remote_port config) outChan 
    forkIO $ processInputMessages mq (pid config) lts inChan outChan
    forever $ runMessageSource (pid config) lts outChan
     
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: String -> MVar LTS.Lts -> Chan S.ByteString -> IO ()
runMessageSource serverId lts chan = do
     msgStr <- encodeMessage <$> composeRequestMessage lts serverId
     writeChan chan msgStr
     liftIO $ threadDelay 3000000

composeRequestMessage :: MVar LTS.Lts -> String -> IO Message
composeRequestMessage lts pid = composeMessage Nothing lts pid Nothing Request

composeMessage :: Maybe String -> MVar LTS.Lts -> String -> Maybe Integer -> Type -> IO Message
composeMessage sourceRequestId lts pid msgTs msgType = do
    ltsVal <- takeMVar lts
    let updatedLtsVal = maybe (LTS.touch ltsVal) (LTS.update ltsVal . LTS.create) msgTs
    let ts = LTS.peek updatedLtsVal
    putMVar lts updatedLtsVal
    uuid <- newUUID
    return $ Message uuid ts msgType pid sourceRequestId

processInputMessages :: MVar MessagePriorityQueue -> String -> MVar LTS.Lts -> Chan S.ByteString -> Chan S.ByteString -> IO ()
processInputMessages mq serverId lts inChan outChan = forever $ do
    msgStr <- readChan inChan
    let msg = decodeMessage msgStr
    maybe printErr handleMsg msg
      where
        handleMsg m = do
            print m
            case msgType m of 
                Request   -> do
                    mqVal <- takeMVar mq
                    let mqVal' = MQ.insert (timestamp m) m mqVal
                    putMVar mq mqVal'
                    print mqVal'
                    sendMsg Reply $ Just (msgId m)
                Reply     -> sendMsg Release (requestId m)
                _         -> return () 
              where 
                sendMsg t rId = writeChan outChan . encodeMessage =<< composeMessage rId lts serverId (Just(timestamp m)) t
        printErr = print "Corrupted message detected."
      

newUUID :: IO String
newUUID = toString <$> randomIO