module Main where

import OptionTest
import SpliteLinesTest
import ListTest
import ConvertTest
import JsonTest
import CodewarsTest
import Test.HUnit

main :: IO Counts
main = runTestTT (TestList (
    optionCases ++
    spliteLinesCases ++
    listTestCases ++
    convertTestCases ++
    jsonTestCases ++
    codewarsTestCases
    ))