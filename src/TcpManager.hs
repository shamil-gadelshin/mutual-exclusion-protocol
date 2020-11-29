{-# LANGUAGE OverloadedStrings #-}

module TcpManager
    ( runServer
    , runClient
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.Chan



runServer :: String -> IO ()
runServer port = do 
    print $ "Server started on port: " ++ port
    startTCPServer Nothing port talk
      where
        talk s = do
            msg <- recv s 1024
            unless (S.null msg) $ do
              C.putStrLn msg
              sendAll s msg
              talk s

-- from the "network-run" package.
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
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
  

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

sendMessages :: Chan C.ByteString -> Socket -> IO () 
sendMessages chan s = do
    sMsg <- readChan chan
    sendAll s sMsg
    rMsg <- recv s 1024
    putStr "Sent: "
    C.putStrLn rMsg
    liftIO $ threadDelay 500000
    sendMessages chan s

-- from the "network-run" package.
startTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
startTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock