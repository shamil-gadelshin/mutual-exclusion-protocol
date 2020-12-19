module MessageBroker
    ( Broker(..)
    , MessageBroker(..)
    ) where

import Control.Concurrent.Chan
import qualified Data.ByteString as S
import Message

-- The Broker class defines communication abstraction for messages
class Broker a  where
    -- Receives a message from communication channel.
    receive   :: a -> IO (Maybe Message)
    -- Sends a message to all available channels.
    broadcast :: a -> Message -> IO ()
    -- Sends a single message to named channel.
    send      :: a -> String -> Message -> IO ()

-- Message broker implementation based on syncrhonous channels. 
data MessageBroker = MessageBroker { inChan   :: Chan S.ByteString 
                                   , outChans :: [(String, Chan S.ByteString)] 
                                   }

instance Broker MessageBroker where
    receive broker = do
        let msgStr = readChan $ inChan broker
        decodeMessage <$> msgStr
    send broker _ msg = writeChan (snd . head $ outChans broker) (encodeMessage msg)
    broadcast broker msg = send broker "" msg 