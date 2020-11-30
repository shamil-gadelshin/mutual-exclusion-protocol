{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Message
    ( getSampleMessage
    , convertMessage
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

getSampleMessage :: ByteString
getSampleMessage =  LBS.toStrict $ encodePretty $ Message "Server1" "10" Request

convertMessage :: Message -> ByteString
convertMessage =  LBS.toStrict . encodePretty

data Type = Request | Reply | Release deriving (Show, Generic)
instance FromJSON Type
instance ToJSON Type

data Message = Message { id :: String
                       , timestamp :: String
                       , msgType :: Type
                       } deriving (Show)

-- Tell Aeson how to create an Message object from JSON string.
instance FromJSON Message where
     parseJSON (Object v) = Message <$>
                            v .:  "id"  <*>
                            v .:  "timestamp" <*>
                            v .:  "msgType"

-- Tell Aeson how to convert an Message object to a JSON string.
instance ToJSON Message where
     toJSON (Message id timestamp msgType) =
         object [ "id" .= id
                , "timestamp" .= timestamp
                , "msgType" .= msgType]