-- | Lamport timestamp module
--   https://en.wikipedia.org/wiki/Lamport_timestamp

module LTS
  ( touch
  , update
  , new
  , peek
  , create
  , Lts(..)
  ) where

-- | Lamport timestamp type
newtype Lts = Lts Integer deriving (Show, Eq)

-- | Creates new timestamp with default value = 1.
new :: Lts
new = Lts 1

-- | Creates new timestamp.
create :: Integer -> Lts
create = Lts

-- | Gets the current value.
peek :: Lts -> Integer
peek (Lts c) = c

-- | Increments the current timestamp value.
touch :: Lts -> Lts
touch (Lts c) = Lts $ c+1

-- | Increments maximum of the provided timestamp values.
update :: Lts -> Lts -> Lts
update (Lts u) (Lts c) = Lts $ max u c + 1
