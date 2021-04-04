-- | Provides TCP transport.

{-# LANGUAGE OverloadedStrings #-}

module TcpManager
  ( runServer
  , runClient
  ) where

import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Control.Exception         as E
import           Control.Monad             (forever, unless, void, when)
import           Control.Monad.Trans
import qualified Data.ByteString           as S
import qualified Data.ByteString.Char8     as C
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

-- Simple message delimiter.
-- TODO: Refactor message separation.
delimiter :: C.ByteString
delimiter = "####"

-- | Starts a local server on the provided port.
runServer :: String -> Chan C.ByteString-> IO ()
runServer port chan = do
  print $ "Server started on port: " ++ port
  startTCPServer Nothing port talk
    where
      talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          parseMessageBatch msg chan
          talk s

-- Parses an input string and sends the message to the channel.
parseMessageBatch :: C.ByteString -> Chan C.ByteString-> IO ()
parseMessageBatch msg chan = do
  unless (C.null msg) $
    case C.breakSubstring delimiter msg of
      (x,xs) | C.null xs -> do
                writeChan chan x
             | otherwise -> do
                unless (C.null x) $
                   writeChan chan x
                parseMessageBatch (C.drop (C.length delimiter) xs) chan

-- Starts a local server on the provided port (Inner function).
-- (from the "network-run" package)
startTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
startTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket
                (addrFamily addr)
                (addrSocketType addr)
                (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

-- | Starts a local client on the provided port.
runClient :: String -> Chan C.ByteString-> IO ()
runClient port chan = do
  print $ "Client connects to port: " ++ port
  E.catch startClient handler
  where
    startClient = startTCPClient "127.0.0.1" port $ sendMessages chan
    handler :: E.SomeException -> IO ()
    handler ex = do
      putStrLn $ "Caught exception: " ++ show ex
      liftIO $ threadDelay 500000
      E.catch startClient handler

-- Sends a message from a channel to a TCP-socket.
sendMessages :: Chan C.ByteString -> Socket -> IO ()
sendMessages chan s = do
  sMsg <- readChan chan
  sendAll s sMsg
  sendAll s delimiter
  sendMessages chan s

-- Starts a local client on the provided port (Inner function).
-- (from the "network-run" package)
startTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
startTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket
                (addrFamily addr)
                (addrSocketType addr)
                (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
