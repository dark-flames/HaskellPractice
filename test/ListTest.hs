module ListTest(listTestCases) where

import Test.HUnit
import List
import Option

listLengthTest = TestCase (assertEqual "Test listLength" 4 (listLength [1, 1, 1, 1]))


func:: Int -> Int
func a = a + 1
listMapTest = TestCase (assertEqual "Test listMap" [2, 3, 4, 5] (listMap [1, 2, 3, 4] func))

headOptionTest1 = TestCase (assertEqual "Test headOption for empty list" (None) (headOption ([]::[Int])))
headOptionTest2 = TestCase (assertEqual "Test headOption for [1, 2, 3]" (Some 1) (headOption [1, 2, 3]))

listReverseTest = TestCase (assertEqual "Test listReverse" [4, 3, 2, 1] (listReverse [1, 2, 3, 4]))

listTakeTest1 = TestCase (assertEqual "Test listTake for empty list" [] (listTake 100 []::[Int]))
listTakeTest2 = TestCase (assertEqual "Test listTake" [1, 2, 3] (listTake 3 [1, 2, 3, 4, 5]))

spliteAtTest = TestCase (assertEqual "Test spliteAt" ([1, 2, 3], [4, 5]) (spliteAt 3 [1, 2, 3, 4, 5]))


listTestCases = [
    listLengthTest,
    listMapTest, 
    headOptionTest1, 
    headOptionTest2, 
    listReverseTest, 
    listTakeTest1, 
    listTakeTest2,
    spliteAtTest]