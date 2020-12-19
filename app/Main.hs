{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Zip
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Foldable
import Text.Printf   

import qualified Data.Text.IO as T
import qualified Data.ByteString as S

import Message;
import MessageBroker;
import qualified CriticalSection as CS;
import RedisManager;
import Config
import TcpManager;
import qualified LTS;
import qualified LME;

-- Extra:
    -- add distributed process transport
    -- add file as protected resource
    -- add formatting
    -- add linting
    -- add comments
    -- decide on MaybeT
    -- decide on import clauses
    -- add basic tests (quickcheck)
    -- add full test script
    -- fix all TODOs
    -- decide on module size and structure
    -- clean project out of useless files
    -- set package configuration
    -- refactor main algorithm

-- Done:
    -- add composed message
    -- catch 'not connected exception' for tcp client
    -- add lamport timestamps
    -- add config from json for remote servers

main :: IO ()
main = do
    -- get configuration
    config <- loadConfigurationFromFile
    let serverId = pid $ local config
    let localPort = port $ local config
    let remoteServers = remotes config
    printf "%s started\n" serverId

    -- construct the message transport
    outChans <- mapM (const newChan) remoteServers

    let processIds = mapM pid remoteServers
    let namedChans = mzip processIds outChans
    inChan <- newChan
    let broker = MessageBroker inChan namedChans

    -- initialize the main algorithm
    lme <- LME.new serverId broker

    -- setup working threads
        -- fork threads for message transport
            -- setup server thread
    forkIO $ runServer localPort inChan

            -- setup client threads
    let remotePorts = map port remoteServers
    let portsAndChans = mzip remotePorts outChans
    mapM_ (\(port, chan) -> forkIO $ runClient port chan) portsAndChans

        -- fork algorithm working thread
    forkIO $ forever $ LME.processInputMessage lme

    -- initialize task creator
    forever $ runMessageSource lme

            
     
--    forever getRedisInfo
--    forever updateRedis

runMessageSource :: (Broker br) => LME.Lme br CS.DummyResource -> IO ()
runMessageSource lme = do
     LME.request lme $ CS.DummyResource "Dummy"
     liftIO $ threadDelay 3000000

