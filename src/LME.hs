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
import Data.Maybe
import qualified Data.HashMap.Strict as HM;
import System.Random
import Control.Lens
import Control.Lens.Prism
import qualified Data.PQueue.Prio.Min as MQ

import qualified Message as M;
import qualified CriticalSection as CS;
import qualified MessageBroker as MB;
import qualified LTS;


type MessagePriorityQueue = MQ.MinPQueue Integer M.Message

-- TODO: gadt 
data Lme b cs = Lme { boxed  :: MVar (LamportMutualExclusion cs)
                    , broker :: b
                    }

data LamportMutualExclusion cs = LamportMutualExclusion
    { _lts       :: LTS.Lts
    , serverId   :: String
    , _queue     :: MessagePriorityQueue
    , _resources :: HM.HashMap String cs
    -- Replies = map (requestId map (serverId replied flag))
    -- Collects responses from other servers for local requests
    -- TODO create separate object with handling methods
    , _replies   :: HM.HashMap String (HM.HashMap String Bool)
    } deriving (Show)

$(makeLenses ''LamportMutualExclusion)

new :: (MB.Broker br, CS.CriticalSection cs) => String -> br -> IO (Lme br cs)
new serverId b = do
    lme <- newMVar $ LamportMutualExclusion LTS.new serverId MQ.empty HM.empty HM.empty
    return $ Lme lme b
 
request :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs -> cs -> IO ()
request lmeObj critSect = do
    msg <- composeMessage lmeObj Nothing Nothing M.Request
    lme <- takeMVar $ boxed lmeObj
    let resources' = HM.insert (M.msgId msg) critSect (lme ^. resources) 
    -- TODO extract separate function
    let replies' = HM.insert (M.msgId msg) (HM.fromList $ zip (MB.peers $ broker lmeObj) (repeat False)) (lme ^. replies) 
    let lme' = (resources .~ resources') . (replies .~ replies') $ lme
    print replies'
    putMVar (boxed lmeObj) lme'
    MB.broadcast (broker lmeObj) msg

-- TODO: optimize boxing unboxing of MVar
composeMessage :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs-> Maybe String -> Maybe Integer -> M.Type -> IO M.Message
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

-- TODO: move boxing-unboxing to the outer scope
handleMessage :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs-> M.Message -> IO (Maybe M.Message)
handleMessage lmeObj msg = do
    case M.msgType msg of 
        M.Request -> do
            lme <- takeMVar (boxed lmeObj)
            let queueVal = lme ^. queue 
            let queue' = MQ.insert (M.timestamp msg) msg queueVal
            let lme' = (queue .~ queue') lme
            putMVar (boxed lmeObj) lme'
            createMsg M.Reply (Just (M.msgId msg))
        M.Reply   -> do 
            lme <- takeMVar (boxed lmeObj)
            let repliesOuterMap = lme ^. replies
            let requestId = fromJust $ M.requestId msg
            let repliesInnerMap = repliesOuterMap HM.! requestId
            let replies' = HM.insert requestId (HM.insert (M.serverId msg) True repliesInnerMap) repliesOuterMap
            let lme' = (replies .~ replies') lme
            putMVar (boxed lmeObj) lme'

            -- TODO execute critical section.

            createMsg M.Release (M.requestId msg)
        M.Release -> do
            lme <- takeMVar (boxed lmeObj)
            let repliesMap = lme ^. replies
            let requestId = fromJust $ M.requestId msg
            let replies' = HM.delete requestId repliesMap

            let queueVal = lme ^. queue 
            let ((_,firstMsg), queue') = MQ.deleteFindMin queueVal
            when ((M.msgId firstMsg) /= requestId) 
                (return $ error "Unexpected first message in the queue on release.")
            
            let lme' = (replies .~ replies') . (queue .~ queue') $ lme
            putMVar (boxed lmeObj) lme'          

            return Nothing 
        where 
            createMsg t rid = Just <$> composeMessage lmeObj rid (Just(M.timestamp msg)) t

-- TODO: create separate functions for map
processInputMessage :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs-> IO ()
processInputMessage lmeObj = do
    let br = broker lmeObj
    msg <- MB.receive br
    maybe printErr handleMsg msg
      where
        handleMsg m = do
            print m
            newMsg <- handleMessage lmeObj m
            forM_ newMsg $ sendMsg (broker lmeObj) (M.serverId m) 
        printErr = print "Corrupted message detected."
        sendMsg br inputServerId msg' = do
            case M.msgType msg' of 
                 M.Reply   -> MB.send br inputServerId msg'
                 M.Release -> MB.broadcast br msg'
                 _         -> return ()