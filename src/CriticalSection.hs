-- | Presents a concept of protected resource and a couple of test examples.

module CriticalSection
    ( CriticalSection(..)
    , DummyResource(..)
    , ProtectedCounter(..)
    ) where

import Control.Concurrent.MVar

-- | The CriticalSection class represents a resource that needs protection.
class CriticalSection a  where
    -- Access the protected resource and/or execute an action on it.
    execute   :: a -> IO ()

-- | CriticalSection example - prints message. 
-- Execution wrapped as a protected resource.
newtype DummyResource = DummyResource String deriving (Show)
instance CriticalSection DummyResource where
    execute dummy = print dummy

-- | CriticalSection example - increments a counter. 
-- Execution wrapped as a protected resource.
newtype ProtectedCounter = ProtectedCounter { counter :: MVar Integer }
instance CriticalSection ProtectedCounter where
    execute pc = do
      value <- takeMVar (counter pc)
      putMVar (counter pc) (value + 1)