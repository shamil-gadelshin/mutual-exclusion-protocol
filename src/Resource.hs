-- | Implements several resources wrapped in the critical section.

module Resource
  (
   DummyResource(..)
  , ProtectedCounter(..)
  ) where

import           Control.Concurrent.MVar
import qualified CriticalSection         as CS


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
