module ConvertTest(convertTestCases) where

import Test.HUnit
import Convert

asIntTest = TestCase (assertEqual "Test listMap" 114514 (asInt "114514"))

convertTestCases = [asIntTest]