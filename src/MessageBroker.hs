-- | Provides high-level transport abstraction.

module MessageBroker
  ( Broker(..)
  , MessageBroker
  , new
  ) where

import           Control.Concurrent.Chan
import qualified Data.ByteString         as S
import qualified Data.HashMap.Strict     as HM
import           Message

-- | The Broker class defines communication abstraction for messages
class Broker a  where
  -- Receives a message from communication channel.
  receive   :: a -> IO (Maybe Message)
  -- Sends a message to all available channels.
  broadcast :: a -> Message -> IO ()
  -- Sends a single message to named channel.
  send      :: a -> String -> Message -> IO ()
  -- Returns peers ids
  peers     :: a -> [String]

-- | Message broker implementation based on syncrhonous channels.
data MessageBroker = MessageBroker
    { inChan   :: Chan S.ByteString
    , outChans :: HM.HashMap String (Chan S.ByteString)
    }

-- | Constructs a new message broker from the IN- and OUT- channels.
new :: Chan S.ByteString -> [(String, Chan S.ByteString)] -> MessageBroker
new inChan chans = MessageBroker inChan (HM.fromList chans)

instance Broker MessageBroker where
  receive broker = do
    let msgStr = readChan $ inChan broker
    decodeMessage <$> msgStr
  send broker serverId msg = do
    let chan = outChans broker HM.! serverId
    writeChan chan (encodeMessage msg)
  broadcast broker msg = mapM_
                          (\pid -> send broker pid msg)
                          (HM.keys $ outChans broker)
  peers broker = HM.keys $ outChans broker
