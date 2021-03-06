module CodewarsTest(codewarsTestCases) where

import Test.HUnit
import CodewarsProblems

duplicateCountTest = TestCase (assertEqual "Test duplicateCount" 3 (duplicateCount "Aa114514"))

sortOddTest = TestCase (assertEqual "Test sort odd" [1, 8, 3, 6, 5, 4, 7, 2, 9, 0] (sortOdd [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] ))

maxSequenceTest = TestCase (assertEqual "Test maxSequence" 6 (maxSequence [-2, 1, -3, 4, -1, 2, 1, -5, 4]))

codewarsTestCases = [
    duplicateCountTest,
    sortOddTest,
    maxSequenceTest]