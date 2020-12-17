module List(listLength, listMap, listFilter, headOption, listReverse, listTake, spliteAt) where

import Option

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = (listLength xs) + 1

listMap :: [a] -> (a -> b) -> [b]
listMap [] _ = []
listMap (item : rest) func = func(item) : (listMap rest func)

listFilter :: [a] -> (a -> Bool) -> [a]
listFilter [] _ = []
listFilter (item : rest) filter = if filter item then
        item : (listFilter rest filter)
    else
        (listFilter rest filter)

headOption :: [a] -> Option a
headOption [] = None
headOption (head : tail) = Some head

listTail :: [a] -> [a]
listTail [] = []
listTail (head : rest) = rest

listReverse:: [a] -> [a]
listReverse [] = []
listReverse (item : rest) = (listReverse rest) ++ [item]


listTake:: Int -> [a] -> [a]
listTake _ [] = []
listTake 0 _ = []
listTake n (item : rest) = item:(listTake (n - 1) rest)


spliteAt:: Int -> [a] -> ([a], [a])
spliteAt 0 list = ([], list)
spliteAt _ [] = ([], [])
spliteAt pos list = if pos > (listLength list) then
    (list, [])
    else let (item : rest) = list
    in let (prefix, suffix) = (spliteAt (pos - 1) rest)
    in (item : prefix, suffix)