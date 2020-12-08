{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-cse #-}

module Config
    ( loadConfigurationFromFile
    , Configuration(..)
    ) where

import Data.ByteString.Char8 as Char8
import System.Console.CmdArgs
import Data.Aeson
import Control.Exception
import Control.Applicative
import Data.ByteString
import Data.Text.Lazy
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as LBS

-- stack build --exec "redisClient-exe --config=./configs/server1.json"
newtype CommandLineArguments = CommandLineArguments {
                                config :: String
                            } deriving (Show, Data, Typeable)

data Configuration = Configuration { pid         :: String
                                   , local_port  :: String
                                   , remote_port :: String
                                   } deriving (Show)

loadConfigurationFromFile :: IO Configuration
loadConfigurationFromFile = do 
    cla <- cmdArgs CommandLineArguments { config = def}
    let fileName = config cla
    json <- catch (decodeFileStrict fileName :: IO (Maybe Configuration))  printLoadErr
    return $ fromMaybe printDecodeErr json
        where 
            printLoadErr :: SomeException -> IO (Maybe Configuration)
            printLoadErr = error "Cannot load configuration file."
            printDecodeErr = error "Cannot decode configuration file."

-- Tell Aeson how to create an Configuration object from JSON string.
instance FromJSON Configuration where
     parseJSON (Object v) = Configuration <$>
                            v .:  "pid"  <*>
                            v .:  "local_port" <*>
                            v .:  "remote_port"

-- Tell Aeson how to convert an Configuration object to a JSON string.
instance ToJSON Configuration where
     toJSON (Configuration pid local_port remote_port) =
         object [ "pid" .= pid
                , "local_port" .= local_port 
                , "remote_port" .= remote_port 
                ]
