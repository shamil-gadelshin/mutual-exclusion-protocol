{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- {-# OPTIONS_GHC -fno-cse #-}

-- | Defines the command line argument for an application and
-- configuration parameters.

module Config
  ( loadConfigurationFromFile
  , Configuration(..)
  , ServerCfg(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe
import           Data.Text.Lazy
import           System.Console.CmdArgs

-- Wrapper-type for command line arguments.
-- Usage: stack build --exec "mep-exe --config=./configs/server1.json"
newtype CommandLineArguments = CommandLineArguments {
                                config :: String -- wrapped config string
                            } deriving (Show, Data, Typeable)

-- | Cluster configuration.
data Configuration = Configuration
    { local   :: ServerCfg -- local node
    -- remote nodes
    , remotes :: [ServerCfg] -- remote nodes
    }
    deriving (Show)

-- | Node configuration.
data ServerCfg = ServerCfg
    { pid  :: String -- unique ID
    -- TCP-port for local runs
    , port :: String -- TCP-port for local runs
    }
    deriving (Show)


-- | Loads a configuration from the provided config-file name.
loadConfigurationFromFile :: IO Configuration
loadConfigurationFromFile = do
  cla <- cmdArgs CommandLineArguments { config = def}
  let fileName = config cla
  json <- catch
            (decodeFileStrict fileName :: IO (Maybe Configuration))
            printLoadErr
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
                        v .:  "remotes"

-- Tell Aeson how to convert an Configuration object to a JSON string.
instance ToJSON Configuration where
  toJSON (Configuration local remotes) =
      object [ "local" .= local
            , "remotes" .= remotes
            ]
