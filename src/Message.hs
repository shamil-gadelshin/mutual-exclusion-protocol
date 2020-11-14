{-# LANGUAGE OverloadedStrings #-}

module Message
    ( getSampleMessage
    , convertMessage
    , Message(..)
    ) where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS

getSampleMessage :: ByteString
getSampleMessage =  LBS.toStrict $ encodePretty $ Message "Server1" "10"

convertMessage :: Message -> ByteString
convertMessage =  LBS.toStrict . encodePretty

data Message = Message { id :: String
                       , timestamp :: String
                       } deriving (Show)

-- Tell Aeson how to create an Message object from JSON string.
instance FromJSON Message where
     parseJSON (Object v) = Message <$>
                            v .:  "id"  <*>
                            v .:  "timestamp"

-- Tell Aeson how to convert an Message object to a JSON string.
instance ToJSON Message where
     toJSON (Message id timestamp) =
         object ["id" .= id,
                 "timestamp" .= timestamp
                 ]