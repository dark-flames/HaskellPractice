module Convert(asInt) where

import Data.Char (digitToInt)

asIntLoop:: Int -> String -> Int
asIntLoop carry [] = carry
asIntLoop carry (c:rest) = asIntLoop (carry * 10 + (digitToInt c)) rest

asInt:: String -> Int
asInt str = asIntLoop 0 str