{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-cse #-}

module Config
    ( getConfiguration
    , Configuration(..)
    ) where

import Data.ByteString.Char8 as Char8
import System.Console.CmdArgs

data Configuration = Configuration { pid         :: String
                                   , local_port  :: String
                                   , remote_port :: String
                                   } deriving (Show, Data, Typeable)

getConfiguration = cmdArgs Configuration { pid         = def
                                         , local_port  = def
                                         , remote_port = def }

