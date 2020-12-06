-- | Lamport mutual exclusion algorithm module
--   https://en.wikipedia.org/wiki/Lamport%27s_distributed_mutual_exclusion_algorithm

module LME
    ( 
    ) where

import Control.Concurrent.MVar
import Data.UUID
import System.Random
import qualified Data.PQueue.Prio.Min as MQ

import qualified Message as M;
import qualified LTS;


type MessagePriorityQueue = MQ.MinPQueue Integer M.Message

type Lme = MVar LamportMutualExclusion

data LamportMutualExclusion = LamportMutualExclusion
    { lts      :: LTS.Lts
    , serverId :: String
    , queue    :: MessagePriorityQueue
    } deriving (Show)

new :: String -> IO Lme
new serverId = newMVar $ LamportMutualExclusion LTS.new serverId MQ.empty
 
request :: IO Lme -> IO M.Message
request lme = composeMessage Nothing lme Nothing M.Request

composeMessage :: Maybe String -> IO Lme -> Maybe Integer -> M.Type -> IO M.Message
composeMessage sourceRequestId lmeBoxed msgTs msgType = do
    lme <- takeMVar =<< lmeBoxed
    let ltsVal = lts lme
    let lts' = maybe (LTS.touch ltsVal) (LTS.update ltsVal . LTS.create) msgTs
    let ts = LTS.peek lts'
    let lme' = LamportMutualExclusion lts' (serverId lme) (queue lme) -- change to lenses
    putMVar <$> lmeBoxed <*> return lme'
    uuid <- newUUID
    return $ M.Message uuid ts msgType (serverId lme) sourceRequestId


newUUID :: IO String
newUUID = toString <$> randomIO