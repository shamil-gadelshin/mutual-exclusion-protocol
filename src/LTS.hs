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

-- Lamport timestamp type
newtype Lts = Lts Integer deriving (Show)

-- creates new timestamp with default value = 1
new :: Lts 
new = Lts 1

-- creates new timestamp
create :: Integer -> Lts 
create = Lts

-- gets the current value
peek :: Lts -> Integer
peek (Lts c) = c

-- increments the current timestamp value
touch :: Lts -> Lts
touch (Lts c) = Lts $ c+1

-- increments maximum of the provided timestamp values
update :: Lts -> Lts -> Lts
update (Lts u) (Lts c) = Lts $ max u c + 1