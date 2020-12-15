module SpliteLinesTest(spliteLinesCases) where

import SpliteLines
import Test.HUnit

mapFunc num = num == 0
testCase = TestCase (
    assertEqual "Test optionMap for Some(False)"
    ["a", "b", "c", "d"]
    (spliteLines "a\nb\rc\r\nd"))

spliteLinesCases = [testCase]
