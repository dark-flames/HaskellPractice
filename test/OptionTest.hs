module OptionTest(optionCases) where

import Option
import Test.HUnit

testIsSomeTrue = TestCase (assertEqual "Test isSome for NoNone" True (isSome(Some "test")))

testIsSomeFalse = TestCase (assertEqual "Test isSome for None" False (isSome(None)))

mapFunc num = num == 0
testOptionMapFalse = TestCase (
    assertEqual "Test optionMap for Some(False)"
    (Some False)
    (optionMap (Some 1) mapFunc))

testOptionMapTrue = TestCase (
    assertEqual "Test optionMap for Some(True)"
    (Some True)
    (optionMap (Some 0) mapFunc))

testOptionMapNone = TestCase (
    assertEqual "Test optionMap for None"
    (None)
    (optionMap (None) mapFunc))

optionCases = [
    testIsSomeTrue,
    testIsSomeFalse,
    testOptionMapFalse, 
    testOptionMapTrue, 
    testOptionMapNone]


