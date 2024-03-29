{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the main protocol message format.

module Message
  ( encodeMessage
  , decodeMessage
  , Type(..)
  , Message(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           GHC.Generics

-- | Encodes a message to a string.
encodeMessage :: Message -> ByteString
encodeMessage =  LBS.toStrict . encodePretty

-- | Constructs a message object from a string.
decodeMessage :: ByteString -> Maybe Message
decodeMessage =  decode . LBS.fromStrict

-- | Defines message type: request resource the cluster, reply from peer and
-- release the resource.
data Type = Request
    | Reply
    | Release
    deriving (Show, Eq, Generic)

instance FromJSON Type
instance ToJSON Type

-- | Defines protocol message.
data Message = Message
    { msgId     :: String
    -- | Lamport timestamp
    , timestamp :: Integer
    -- | Message type
    , msgType   :: Type
    -- | Peer server ID
    , serverId  :: String
    -- | Source request id
    , requestId :: Maybe String
    }
    deriving (Show, Eq)

-- | Tell Aeson how to create an Message object from JSON string.
instance FromJSON Message where
  parseJSON (Object v) = Message <$>
                        v .:  "msgId"  <*>
                        v .:  "timestamp" <*>
                        v .:  "msgType" <*>
                        v .:  "serverId" <*>
                        v .:  "requestId"

-- | Tell Aeson how to convert an Message object to a JSON string.
instance ToJSON Message where
  toJSON (Message msgId timestamp msgType serverId requestId) =
    object [ "msgId" .= msgId
          , "timestamp" .= timestamp
          , "msgType" .= msgType
          , "serverId" .= serverId
          , "requestId" .= requestId
          ]
