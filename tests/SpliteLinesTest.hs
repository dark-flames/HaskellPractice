module SpliteLinesTest(spliteLinesCases) where

import SpliteLines
import Test.HUnit

testCase = TestCase (
    assertEqual "Test slite lines"
    ["a", "b", "c", "d"]
    (spliteLines "a\nb\rc\r\nd"))

spliteLinesCases = [testCase]
