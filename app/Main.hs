{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Zip
import           Data.Foldable
import           System.Random
import           Text.Printf

import qualified Data.ByteString         as S
import qualified Data.Text.IO            as T

import           Config
import qualified LME
import qualified LTS
import           Message
import qualified MessageBroker           as MB
import qualified Resource                as R
import           TcpManager

-- Extra:
    -- add distributed process transport
    -- add file as protected resource
    -- add redis counter as protected resource
    -- consider MaybeT
    -- add host to config for a distributed run.

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

  let processIds = map pid remoteServers
  print processIds
  let namedChans = mzip processIds outChans
  inChan <- newChan
  let broker = MB.new inChan namedChans

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
  forkIO $ forever $ LME.runMessagePipeline lme

  -- random delay before request generation to avoid initial LTS overlap
  randomDelay <- randomRIO (2000000, 6000000) -- from 2 to 6 sec
  liftIO $ threadDelay randomDelay

  -- initialize task creator
  --forever $ runMessageSource lme
  replicateM_ 100 $ runMessageSource lme

  print "Done."
  liftIO $ threadDelay 100000000 -- 100 seconds


runMessageSource :: (MB.Broker br) => LME.Lme br R.RedisCounter -> IO ()
runMessageSource lme = do
  config <- loadConfigurationFromFile
  let localPort = port $ local config

  LME.request lme $ R.RedisCounter "YCounter"

  liftIO $ threadDelay 500000 -- 0.5 sec
