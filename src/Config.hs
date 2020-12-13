{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-cse #-}

module Config
    ( loadConfigurationFromFile
    , Configuration(..)
    , ServerCfg(..)
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

data Configuration = Configuration { local       :: ServerCfg
                                   , remote_port :: String
                                   } deriving (Show)

data ServerCfg = ServerCfg { pid         :: String
                           , port  :: String
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

-- Tell Aeson how to create an ServerCfg object from JSON string.
instance FromJSON ServerCfg where
     parseJSON (Object v) = ServerCfg <$>
                            v .:  "pid"  <*>
                            v .:  "port"

-- Tell Aeson how to convert an ServerCfg object to a JSON string.
instance ToJSON ServerCfg where
     toJSON (ServerCfg pid port) =
         object [ "pid" .= pid
                , "port" .= port 
                ]

-- Tell Aeson how to create an Configuration object from JSON string.
instance FromJSON Configuration where
     parseJSON (Object v) = Configuration <$>
                            v .:  "local" <*>
                            v .:  "remote_port"

-- Tell Aeson how to convert an Configuration object to a JSON string.
instance ToJSON Configuration where
     toJSON (Configuration local remote_port) =
         object [ "local" .= local 
                , "remote_port" .= remote_port 
                ]
