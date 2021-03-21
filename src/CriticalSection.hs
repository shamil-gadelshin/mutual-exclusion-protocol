module CriticalSection
    ( CriticalSection(..)
    , DummyResource(..)
    , ProtectedCounter(..)
    ) where

import Control.Concurrent.MVar

-- The CriticalSection class represents a resource that needs protection.
class CriticalSection a  where
    -- Access the protected resource and/or execute an action on it.
    execute   :: a -> IO ()

-- Test resource printing for a critical section.
newtype DummyResource = DummyResource String deriving (Show)
instance CriticalSection DummyResource where
    execute dummy = print dummy

-- Test resource counter with a critical section.
newtype ProtectedCounter = ProtectedCounter { counter :: MVar Integer }
instance CriticalSection ProtectedCounter where
    execute pc = do
      value <- takeMVar (counter pc)
      putMVar (counter pc) (value + 1)