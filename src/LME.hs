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
import Data.UUID
import System.Random
import Control.Lens
import Control.Lens.Prism
import qualified Data.PQueue.Prio.Min as MQ

import qualified Message as M;
import qualified LTS;


type MessagePriorityQueue = MQ.MinPQueue Integer M.Message

type Lme = MVar LamportMutualExclusion

data LamportMutualExclusion = LamportMutualExclusion
    { _lts      :: LTS.Lts
    , serverId :: String
    , _queue    :: MessagePriorityQueue
    } deriving (Show)

$(makeLenses ''LamportMutualExclusion)

new :: String -> IO Lme
new serverId = newMVar $ LamportMutualExclusion LTS.new serverId MQ.empty
 
request :: Lme -> IO M.Message
request lme = composeMessage lme Nothing Nothing M.Request

composeMessage :: Lme -> Maybe String -> Maybe Integer -> M.Type -> IO M.Message
composeMessage lmeBoxed sourceRequestId msgTs msgType = do
    lme <- takeMVar lmeBoxed
    let ltsVal = lme ^. lts
    let lts' = maybe (LTS.touch ltsVal) (LTS.update ltsVal . LTS.create) msgTs
    let ts = LTS.peek lts'
    let lme' = (lts .~ lts') lme
    putMVar lmeBoxed lme'
    uuid <- newUUID
    return $ M.Message uuid ts msgType (serverId lme) sourceRequestId


newUUID :: IO String
newUUID = toString <$> randomIO

processInputMessage :: Lme -> M.Message -> IO (Maybe M.Message)
processInputMessage lmeBoxed msg = do
    case M.msgType msg of 
        M.Request -> do
            lme <- takeMVar lmeBoxed
            let mqVal = lme ^. queue 
            let queue' = MQ.insert (M.timestamp msg) msg mqVal
            let lme' = (queue .~ queue') lme
            putMVar lmeBoxed lme'
            createMsg M.Reply (Just (M.msgId msg))
        M.Reply   -> createMsg M.Release (M.requestId msg)
        _         -> return Nothing 
        where 
            createMsg t rid = Just <$> composeMessage lmeBoxed rid (Just(M.timestamp msg)) t