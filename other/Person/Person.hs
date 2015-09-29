module Person (
  Person(..),
  sortPeople,
  peopleWith
) where

import           Data.Function
import           Data.List

data Person = Person {
firstName :: String,
lastName  :: String,
city      :: String,
state     :: String,
address   :: String,
zip       :: String,
age       :: Int
} deriving (Show, Ord, Eq)

sortPeople :: [Person] -> [Person]
sortPeople = sortBy (compare `on` lastName)

peopleWith :: (Eq string) => (Person -> string) -> string -> [Person] -> [Person]
peopleWith func target ps = [ p | p <- ps, func p == target ]
