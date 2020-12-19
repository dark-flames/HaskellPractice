module ListTest(listTestCases) where

import Test.HUnit
import List
import Option

listLengthTest = TestCase (assertEqual "Test listLength" 4 (listLength [1, 1, 1, 1]))


listMapTest = TestCase (assertEqual "Test listMap" [2, 3, 4, 5] (listMap [1, 2, 3, 4] func))
    where func a = a + 1

listFilterTest = TestCase (assertEqual "Test listFilter" [1, 3] (listFilter [1, 2, 3, 4] odd))

listReduceTest = TestCase (assertEqual "Test reduce" 15 (listReduce [1, 2, 3, 4, 5] foldFunc 0))
    where foldFunc a b = a + b

headOptionTest1 = TestCase (assertEqual "Test headOption for empty list" (None) (headOption ([]::[Int])))
headOptionTest2 = TestCase (assertEqual "Test headOption for [1, 2, 3]" (Some 1) (headOption [1, 2, 3]))

listReverseTest = TestCase (assertEqual "Test listReverse" [4, 3, 2, 1] (listReverse [1, 2, 3, 4]))

listTakeTest1 = TestCase (assertEqual "Test listTake for empty list" [] (listTake 100 []::[Int]))
listTakeTest2 = TestCase (assertEqual "Test listTake" [1, 2, 3] (listTake 3 [1, 2, 3, 4, 5]))

spliteAtTest = TestCase (assertEqual "Test spliteAt" ([1, 2, 3], [4, 5]) (spliteAt 3 [1, 2, 3, 4, 5]))


listTestCases = [
    listLengthTest,
    listMapTest, 
    listFilterTest,
    listReduceTest,
    headOptionTest1, 
    headOptionTest2, 
    listReverseTest, 
    listTakeTest1, 
    listTakeTest2,
    spliteAtTest]