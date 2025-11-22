module Utils where

import Data.List (sortBy, group, sort)
import Data.Ord (comparing)

-- | Generic sort function
sortList :: Ord a => [a] -> [a]
sortList = sort

-- | Counts the frequency of each character in a string
--   Returns a list of (Char, Frequency) tuples
frequencyCount :: String -> [(Char, Int)]
frequencyCount str = map (\x -> (head x, length x)) grouped
  where
    grouped = group (sort str)

-- | Sorts a list of tuples based on the frequency (second element)
sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq = sortBy (comparing snd)