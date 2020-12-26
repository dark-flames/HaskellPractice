module JsonTest(jsonTestCases) where

import Test.HUnit
import Json
testCaseShow = JsonObject [
    ("List1", JsonList [JsonBool True, JsonBool False, JsonNull, JsonNumber 114514]),
    ("List2", JsonList [JsonList [JsonBool True, JsonBool False], JsonObject [("a", JsonString "a")]]),
    ("bool", JsonBool True),
    ("double", JsonNumber 114.514)]

testCaseShowResult = "{\"List1\":[true,false,null,114514],\"List2\":[[true,false],{\"a\":\"a\"}],\"bool\":true,\"double\":114.514}"

showTest = TestCase (assertEqual "Test json show" testCaseShowResult (show testCaseShow))

getCharTest1 = TestCase (assertEqual "Test get char" (Just "\\n", " aa") (getCharacter "\\n aa"))
getCharTest2 = TestCase (assertEqual "Test get char" (Just "\\n", "") (getCharacter "\\n"))
getCharTest3 = TestCase (assertEqual "Test get char" (Nothing , "") (getCharacter ""))

getAsStrTest1 = TestCase (assertEqual "Test get as str" (Nothing , "abc\\\"d") (getAsStr "abc\\\"d"))
getAsStrTest2 = TestCase (assertEqual "Test get as str" (Just "abc" , "d") (getAsStr "abc\"d"))
getAsNumTest1 = TestCase (assertEqual "Test get as num" ("114.514" , "asdasd") (getAsNum "114.514asdasd"))
getAsNumTest2 = TestCase (assertEqual "Test get as num" ("114.514" , "") (getAsNum "114.514"))

getAsNumTest3 = TestCase (assertEqual "Test get as num" ("" , "aa114.514") (getAsNum "aa114.514"))

testParse = TestCase (assertEqual "Test parse" (Just testCaseShow, "") (getItem testCaseShowResult))

testPopChar = TestCase (assertEqual "Test pop char" Nothing (popChar "," "]"))

testReadPair = TestCase (assertEqual "Test read pair" (Just ("a", JsonNumber 1), ",sdasd") (readPair "\"a\": 1,sdasd"))

jsonTestCases = [
    showTest,
    getCharTest1,
    getCharTest2,
    getCharTest3,
    getAsStrTest1,
    getAsStrTest2,
    getAsNumTest1,
    getAsNumTest2,
    getAsNumTest3,
    testPopChar,
    testReadPair,
    testParse]