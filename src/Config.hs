{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-cse #-}

module Config
    ( getConfiguration
    , Configuration(..)
    ) where

import Data.ByteString.Char8 as Char8
import System.Console.CmdArgs

data Configuration = Configuration { pid :: String
                                   , ip :: String
                                   } deriving (Show, Data, Typeable)

getConfiguration = cmdArgs Configuration { pid = def, ip = def }

