module CriticalSection
    ( CriticalSection(..)
    , DummyResource(..)
    ) where

-- The CriticalSection class represents a resource that needs protection.
class Show a => CriticalSection a  where
    -- Access the protected resource and/or execute an action on it.
    execute   :: a -> IO ()

-- Test resource for a critical section.
newtype DummyResource = DummyResource String deriving (Show)
instance CriticalSection DummyResource where
    execute dummy = print dummy