module CodewarsProblems(duplicateCount, sortOdd) where

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