-- | Presents a concept of protected resource.

module CriticalSection
  ( CriticalSection(..)
  ) where

-- | The CriticalSection class represents a resource that needs protection.
class CriticalSection a  where
  -- Access the protected resource and/or execute an action on it.
  execute :: a -> IO ()
