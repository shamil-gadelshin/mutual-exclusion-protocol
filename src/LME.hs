{-# LANGUAGE TemplateHaskell #-}

-- | Lamport mutual exclusion algorithm module.
-- Details: https://en.wikipedia.org/wiki/Lamport%27s_distributed_mutual_exclusion_algorithm

module LME
  ( new
  , request
  , runMessagePipeline
  , Lme
  ) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Lens.Prism
import           Control.Monad
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import qualified Data.PQueue.Prio.Min    as MQ
import           Data.UUID
import           System.Random

import qualified CriticalSection         as CS
import qualified LTS
import qualified Message                 as M
import qualified MessageBroker           as MB

-- Priority queue type-wrapper.
type MessagePriorityQueue = MQ.MinPQueue Integer M.Message

-- TODO: Consider gadt
-- | Lamport mutual exclusion algoritm helper.
data Lme b cs = Lme
    { boxed  :: MVar (LamportMutualExclusion cs)
    -- abstract message broker
    , broker :: b -- abstract message broker
    }

-- Collects replies from other servers for local requests
-- combined by original requests IDs
newtype ServerReplies = ServerReplies
  -- Pseudocode: serverReplies = map (requestId map (serverId repliedFlag))
  { serverReplies :: HM.HashMap String (HM.HashMap String Bool)
  } deriving (Show)

-- Data structure for the Lamport mutual exclusion algorithm.
data LamportMutualExclusion cs = LamportMutualExclusion
    { _lts       :: LTS.Lts -- current Lamport timestamp
    -- local server ID
    , serverId   :: String -- local server ID
    , _queue     :: MessagePriorityQueue
    -- protected resources
    , _resources :: HM.HashMap String cs -- protected resources
    -- replies from the peers
    , _replies   :: ServerReplies -- replies from the peers
    }
    deriving (Show)

$(makeLenses ''LamportMutualExclusion)

-- | Creates a new instance of the Lamport mutual exclusion algorithm.
new :: (MB.Broker br, CS.CriticalSection cs) => String -> br -> IO (Lme br cs)
new serverId b = do
  lme <- newMVar $ LamportMutualExclusion
                    LTS.new
                    serverId
                    MQ.empty
                    HM.empty
                    (ServerReplies HM.empty)
  return $ Lme lme b

-- TODO: consider modifyMVar_
-- | Request an access to the protected resource.
request :: (MB.Broker br, CS.CriticalSection cs) => Lme br cs -> cs -> IO ()
request lmeObj critSect = do
  lme <- takeMVar $ boxed lmeObj
  (msg, lts') <- composeMessage
                    (lme ^. lts)
                    (serverId lme)
                    Nothing
                    Nothing
                    M.Request
  let resources' = HM.insert (M.msgId msg) critSect (lme ^. resources)
  let replies' = createEmptyServerRepliesEntry
                    (lme ^. replies)
                    (M.msgId msg)
                    (MB.peers $ broker lmeObj)

  let queue' = lme ^. queue
  let queue'' = MQ.insert (M.timestamp msg) msg queue'

  let lme' = (resources .~ resources')
                . (replies .~ replies')
                . (queue .~ queue'')
                . (lts .~ lts')
                $ lme
  putMVar (boxed lmeObj) lme'
  MB.broadcast (broker lmeObj) msg

-- Compose a message object from the data.
composeMessage
  :: LTS.Lts        -- current Lamport timestamp for the algorithm
  -> String         -- local server ID
  -> Maybe String   -- source request ID for a message (optional)
  -> Maybe Integer  -- previous message timestamp (optional)
  -> M.Type         -- message type
  -> IO (M.Message, LTS.Lts)
composeMessage lts serverId sourceRequestId msgTs msgType = do
  let lts' = maybe (LTS.touch lts) (LTS.update lts . LTS.create) msgTs
  let ts = LTS.peek lts'
  uuid <- newUUID
  return (M.Message uuid ts msgType serverId sourceRequestId, lts')

-- Generates random UUID
newUUID :: IO String
newUUID = toString <$> randomIO

-- TODO: consider modifyMVar_
-- Processes the inbound message and generates outbound message in some cases
handleMessage
  :: (MB.Broker br, CS.CriticalSection cs)
  => Lme br cs
  -> M.Message
  -> IO (Maybe M.Message)
handleMessage lmeObj msg = do
  lme <- takeMVar (boxed lmeObj)
  (lme', msg') <- case M.msgType msg of
      M.Request -> handleRequest lme msg
      M.Reply   -> handleReply lme msg
      M.Release -> handleRelease lme msg
  putMVar (boxed lmeObj) lme'
  return msg'
  where
    handleRequest lme msg = do
      let queueVal = lme ^. queue
      let queue' = MQ.insert (M.timestamp msg) msg queueVal

      (msg', lts') <- composeMessage
                        (lme ^. lts)
                        (serverId lme)
                        (Just (M.msgId msg))
                        (Just(M.timestamp msg))
                        M.Reply
      let lme' = (queue .~ queue') . (lts .~ lts') $ lme

      return (lme', Just msg')
    handleReply lme msg = do
      let queueVal = lme ^. queue
      let (_, firstMsg) = MQ.findMin queueVal

      let requestId = fromJust $ M.requestId msg

      let replies' = registerServerReply
                        (lme ^. replies)
                        requestId
                        (M.serverId msg)

      if (allReplyReceived replies' requestId) &&
          (M.msgId firstMsg == requestId)
          then do
              let queue' = MQ.deleteMin queueVal

              -- execute critical section.
              CS.execute $ (lme ^. resources) HM.! requestId

              (msg', lts') <- composeMessage
                                (lme ^. lts)
                                (serverId lme)
                                (M.requestId msg)
                                (Just(M.timestamp msg))
                                M.Release
              let replies'' = removeServerReplyEntry replies' requestId
              let lme' =   (replies .~ replies'')
                          . (queue .~ queue')
                          . (lts .~ lts')
                          $ lme

              return (lme', Just msg')
          else do
            let lme' = (replies .~ replies') lme
            return (lme',  Nothing)
    handleRelease lme msg = do
      let requestId = fromJust $ M.requestId msg
      let queueVal = lme ^. queue
      let ((_,firstMsg), queue') = MQ.deleteFindMin queueVal
      when (M.msgId firstMsg /= requestId) $
        return $ error "Unexpected first message in the queue on release."

      let lme' = queue .~ queue' $ lme

      return (lme',  Nothing)


-- | Process a single incoming message. Should be run in a cycle.
runMessagePipeline
  :: (MB.Broker br, CS.CriticalSection cs)
  => Lme br cs
  -> IO ()
runMessagePipeline lmeObj = do
  let br = broker lmeObj
  msg <- MB.receive br
  maybe printErr handleMsg msg
    where
      handleMsg m = do
        print $ "Handling message: " ++ show m
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
createEmptyServerRepliesEntry
  :: ServerReplies
  -> String
  -> [String]
  -> ServerReplies
createEmptyServerRepliesEntry sr msgId peers = ServerReplies $
  HM.insert msgId (HM.fromList $ zip peers (repeat False)) (serverReplies sr)

-- Sets 'server reply' to True
registerServerReply :: ServerReplies -> String -> String -> ServerReplies
registerServerReply sr requestId serverId =
  let repliesOuterMap = serverReplies sr in
  let repliesInnerMap = repliesOuterMap HM.! requestId in
  ServerReplies $ HM.insert
                    requestId
                    (HM.insert serverId True repliesInnerMap)
                    repliesOuterMap

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
