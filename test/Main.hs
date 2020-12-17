module Main where

import OptionTest
import SpliteLinesTest
import ListTest
import Test.HUnit

main :: IO Counts
main = runTestTT (TestList (
    optionCases ++
    spliteLinesCases ++
    listTestCases
    ))