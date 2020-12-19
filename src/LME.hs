{-# LANGUAGE TemplateHaskell #-}

-- | Lamport mutual exclusion algorithm module
--   https://en.wikipedia.org/wiki/Lamport%27s_distributed_mutual_exclusion_algorithm

module LME
    ( new
    , request
    , processInputMessage
    , Lme
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.UUID
import qualified Data.HashMap.Strict as HM;
import System.Random
import Control.Lens
import Control.Lens.Prism
import qualified Data.PQueue.Prio.Min as MQ

import qualified Message as M;
import qualified CriticalSection as CS;
import MessageBroker;
import qualified LTS;


type MessagePriorityQueue = MQ.MinPQueue Integer M.Message

data Lme b cs = Lme { boxed  :: MVar (LamportMutualExclusion cs)
                    , broker :: b
                    }
    

data LamportMutualExclusion cs = LamportMutualExclusion
    { _lts      :: LTS.Lts
    , serverId  :: String
    , _queue    :: MessagePriorityQueue
    , _hashMap   :: HM.HashMap String cs
    } deriving (Show)

$(makeLenses ''LamportMutualExclusion)

new :: (Broker br, CS.CriticalSection cs) => String -> br -> IO (Lme br cs)
new serverId b = do
    lme <- newMVar $ LamportMutualExclusion LTS.new serverId MQ.empty HM.empty
    return $ Lme lme b
 
request :: (Broker br, CS.CriticalSection cs) => Lme br cs -> cs -> IO ()
request lmeObj critSect = do
    msg <- composeMessage lmeObj Nothing Nothing M.Request
    lme <- takeMVar $ boxed lmeObj
    let hashMap' = HM.insert (M.msgId msg) critSect (lme ^. hashMap) 
    let lme' = (hashMap .~ hashMap') lme
    putMVar (boxed lmeObj) lme'
    broadcast (broker lmeObj) msg

-- TODO: optimize boxing unboxing of MVar
composeMessage :: (Broker br, CS.CriticalSection cs) => Lme br cs-> Maybe String -> Maybe Integer -> M.Type -> IO M.Message
composeMessage lmeObj sourceRequestId msgTs msgType = do
    lme <- takeMVar $ boxed lmeObj
    let ltsVal = lme ^. lts
    let lts' = maybe (LTS.touch ltsVal) (LTS.update ltsVal . LTS.create) msgTs
    let ts = LTS.peek lts'
    let lme' = (lts .~ lts') lme
    putMVar (boxed lmeObj) lme'
    uuid <- newUUID
    return $ M.Message uuid ts msgType (serverId lme) sourceRequestId


newUUID :: IO String
newUUID = toString <$> randomIO

handleMessage :: (Broker br, CS.CriticalSection cs) => Lme br cs-> M.Message -> IO (Maybe M.Message)
handleMessage lmeObj msg = do
    case M.msgType msg of 
        M.Request -> do
            lme <- takeMVar (boxed lmeObj)
            let mqVal = lme ^. queue 
            let queue' = MQ.insert (M.timestamp msg) msg mqVal
            let lme' = (queue .~ queue') lme
            putMVar (boxed lmeObj) lme'
            createMsg M.Reply (Just (M.msgId msg))
        M.Reply   -> createMsg M.Release (M.requestId msg)
        _         -> return Nothing -- TODO remove from the queue on receiving the release
        where 
            createMsg t rid = Just <$> composeMessage lmeObj rid (Just(M.timestamp msg)) t

processInputMessage :: (Broker br, CS.CriticalSection cs) => Lme br cs-> IO ()
processInputMessage lmeObj = do
    let br = broker lmeObj
    msg <- receive br
    maybe printErr handleMsg msg
      where
        handleMsg m = do
            print m
            newMsg <- handleMessage lmeObj m
            forM_ newMsg $ sendMsg (broker lmeObj) -- TODO send or broadcast message based on type
        printErr = print "Corrupted message detected."
        sendMsg br msg' = send br "" msg' --TODO