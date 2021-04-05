-- | Implements several resources wrapped in the critical section.

module Resource
  (
   DummyResource(..)
  , ProtectedCounter(..)
  , RedisCounter(..)
  ) where

import           Control.Concurrent.MVar
import           Data.ByteString.Char8

import qualified CriticalSection         as CS
import qualified RedisManager            as RM


-- | CriticalSection example - prints message.
-- Execution wrapped as a protected resource.
newtype DummyResource = DummyResource String deriving (Show)
instance CS.CriticalSection DummyResource where
  execute dummy = print dummy

-- | CriticalSection example - increments a counter.
-- Execution wrapped as a protected resource.
newtype ProtectedCounter = ProtectedCounter { counter :: MVar Integer }
instance CS.CriticalSection ProtectedCounter where
  execute pc = do
    value <- takeMVar (counter pc)
    putMVar (counter pc) (value + 1)

-- | CriticalSection example - increments a redis-based counter.
-- Execution wrapped as a protected resource.
newtype RedisCounter = RedisCounter { counterName :: ByteString }
instance CS.CriticalSection RedisCounter where
  execute rc = do
    RM.incrementCounter $ counterName rc
