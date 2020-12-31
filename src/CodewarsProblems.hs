module CodewarsProblems(
  duplicateCount,
  sortOdd,
  maxSequence) where

import Data.List 
import Data.Maybe
import Data.Char
import Prelude


duplicateCount :: String -> Int
duplicateCount = length . filter (> 1) . map length. group . sort . map toLower

fillList :: [Int] -> [Int] -> ([Int], [Int])
fillList [] _ = ([], [])
fillList list [] = (list, [])
fillList (item:list) (oddItem:oddRest) = if (odd item) then
    let (nextList, nextOdd) = fillList list oddRest
    in (oddItem:nextList, nextOdd)
  else let (nextList, nextOdd) = fillList list (oddItem:oddRest)
    in (item:nextList, nextOdd)

sortOdd :: [Int] -> [Int]
sortOdd list = fst (fillList list (sort (filter odd list)))

-- sum[i] = max{sum[i-1] + value[i], value[i]}
maxSequence :: [Int] -> Int
maxSequence = maximum . scanl (\acc item -> max item (acc + item)) 0
