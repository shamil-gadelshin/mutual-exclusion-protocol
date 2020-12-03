{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Message
    ( encodeMessage
    , decodeMessage
    , Type(..)
    , Message(..)
    ) where

import GHC.Generics
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS

encodeMessage :: Message -> ByteString
encodeMessage =  LBS.toStrict . encodePretty

decodeMessage :: ByteString -> Maybe Message
decodeMessage =  decode . LBS.fromStrict

data Type = Request | Reply | Release deriving (Show, Generic)
instance FromJSON Type
instance ToJSON Type

data Message = Message { msgId     :: String
                       , timestamp :: Integer
                       , msgType   :: Type
                       , serverId  :: String
                       , requestId :: Maybe String -- source request id
                       } deriving (Show)

-- Tell Aeson how to create an Message object from JSON string.
instance FromJSON Message where
     parseJSON (Object v) = Message <$>
                            v .:  "msgId"  <*>
                            v .:  "timestamp" <*>
                            v .:  "msgType" <*>
                            v .:  "serverId" <*>
                            v .:  "requestId"

-- Tell Aeson how to convert an Message object to a JSON string.
instance ToJSON Message where
     toJSON (Message msgId timestamp msgType serverId requestId) =
         object [ "msgId" .= msgId
                , "timestamp" .= timestamp
                , "msgType" .= msgType 
                , "serverId" .= serverId 
                , "requestId" .= requestId 
                ]