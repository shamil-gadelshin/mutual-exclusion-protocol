{-# LANGUAGE TemplateHaskell #-}

-- | Lamport mutual exclusion algorithm module
--   https://en.wikipedia.org/wiki/Lamport%27s_distributed_mutual_exclusion_algorithm

module LME
    ( new
    , request
    , runMessagePipeline
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

-- Collects replies from other servers for local requests 
-- combined by original requests IDs
newtype ServerReplies = ServerReplies
    -- serverReplies = map (requestId map (serverId repliedFlag))
    { serverReplies :: HM.HashMap String (HM.HashMap String Bool)
    } deriving (Show)

data LamportMutualExclusion cs = LamportMutualExclusion
    { _lts       :: LTS.Lts
    , serverId   :: String
    , _queue     :: MessagePriorityQueue
    , _resources :: HM.HashMap String cs
    , _replies   :: ServerReplies
    } deriving (Show)

$(makeLenses ''LamportMutualExclusion)

new :: (MB.Broker br, CS.CriticalSection cs) => String -> br -> IO (Lme br cs)
new serverId b = do
    lme <- newMVar $ LamportMutualExclusion LTS.new serverId MQ.empty HM.empty (ServerReplies HM.empty)
    return $ Lme lme b
 
request :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs -> cs -> IO ()
request lmeObj critSect = do
    msg <- composeMessage lmeObj Nothing Nothing M.Request
    lme <- takeMVar $ boxed lmeObj
    let resources' = HM.insert (M.msgId msg) critSect (lme ^. resources) 
    let replies' = createEmptyServerRepliesEntry (lme ^. replies) (M.msgId msg) (MB.peers $ broker lmeObj)
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
            
            let queueVal = lme ^. queue 
            let (_, firstMsg) = MQ.findMin queueVal

            let requestId = fromJust $ M.requestId msg

            let replies' = registerServerReply (lme ^. replies) requestId (M.serverId msg)

            let result = if allReplyReceived replies' requestId
                            then do
                                if M.msgId firstMsg == requestId
                                then do
                                    let queue' = MQ.deleteMin queueVal
                                    let lme' = (replies .~ (removeServerReplyEntry replies' requestId)) . (queue .~ queue') $ lme
                                    putMVar (boxed lmeObj) lme'
                                    -- TODO execute critical section.

                                    createMsg M.Release (M.requestId msg)
                                else do
                                    let lme' = (replies .~ replies') lme
                                    putMVar (boxed lmeObj) lme'
                                    return Nothing
                            else do
                                let lme' = (replies .~ replies') lme
                                putMVar (boxed lmeObj) lme'
                                return Nothing

            result
        M.Release -> do
            let requestId = fromJust $ M.requestId msg

            lme <- takeMVar (boxed lmeObj)
            let queueVal = lme ^. queue 
            let ((_,firstMsg), queue') = MQ.deleteFindMin queueVal
            when (M.msgId firstMsg /= requestId) 
                (return $ error "Unexpected first message in the queue on release.")
            
            let lme' = queue .~ queue' $ lme
            putMVar (boxed lmeObj) lme'          

            return Nothing 
        where 
            createMsg t rid = Just <$> composeMessage lmeObj rid (Just(M.timestamp msg)) t

runMessagePipeline :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs-> IO ()
runMessagePipeline lmeObj = do
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

-- ServerReplies helper functions

-- Creates new 'server replies' entry for the request. Fills default flag
-- values to 'False' for known peer servers.
createEmptyServerRepliesEntry :: ServerReplies -> String -> [String] -> ServerReplies
createEmptyServerRepliesEntry sr msgId peers = ServerReplies $ 
    HM.insert msgId (HM.fromList $ zip peers (repeat False)) (serverReplies sr)

-- Sets 'server reply' to True
registerServerReply :: ServerReplies -> String -> String -> ServerReplies
registerServerReply sr requestId serverId = 
    let repliesOuterMap = serverReplies sr in
    let repliesInnerMap = repliesOuterMap HM.! requestId in 
    ServerReplies $ 
        HM.insert requestId (HM.insert serverId True repliesInnerMap) repliesOuterMap

-- Looks through server replies for a given request ID. Returns true if all
-- servers replied.
allReplyReceived :: ServerReplies -> String -> Bool
allReplyReceived sr requestId = 
    let replies = serverReplies sr in
    let peersPermissions = replies HM.! requestId in
    let allPermissionReceived = and $ HM.elems peersPermissions
    in allPermissionReceived

-- Deletes 'server reply' entry by the request ID.
removeServerReplyEntry :: ServerReplies -> String -> ServerReplies
removeServerReplyEntry sr requestId = ServerReplies $ 
    HM.delete requestId (serverReplies sr)