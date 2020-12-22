module Json(
    JsonValue(JsonObject, JsonList, JsonString, JsonNumber, JsonBool, JsonNull),
    unwrapAsObject,
    unwrapAsList,
    getCharacter,
    getAsStr,
    getAsNum) where

import Data.List
import Data.Maybe
import Data.Char
import List

type NamedValue = (String, JsonValue)
data JsonValue = JsonObject [NamedValue] 
    | JsonList [JsonValue]
    | JsonString String
    | JsonNumber Double
    | JsonBool Bool
    | JsonNull

showPair :: NamedValue -> String
showPair (key, value)  = "\"" ++ key ++ "\":" ++ show value

commaJoin :: String -> String -> String
commaJoin "" item = item
commaJoin carry item = carry ++ "," ++ item

isInt:: Double -> Bool
isInt x = x == fromInteger (round x)

instance Show JsonValue where
    show JsonNull = "null"
    show (JsonBool nested) = if nested then "true" else "false"
    show (JsonNumber nested) = if isInt nested then show (round nested)else show nested
    show (JsonString nested) = "\"" ++ nested ++ "\""
    show (JsonList list) = "[" ++ content ++ "]" where
        content = listReduce (listMap list show) commaJoin ""
    show (JsonObject objectList) = "{" ++ content ++ "}" where
        content = listReduce (listMap objectList showPair) commaJoin ""

unwrapAsObject :: JsonValue -> Maybe [NamedValue]
unwrapAsObject (JsonObject objectList) = Just objectList
unwrapAsObject _ = Nothing

unwrapAsList :: JsonValue -> Maybe [JsonValue]
unwrapAsList (JsonList list) = Just list
unwrapAsList _ = Nothing


getCharacter :: String -> (Maybe String, String)
getCharacter "" = (Nothing, "")
getCharacter ('\\' : rest) = let 
    (next, nextRest) = getCharacter rest
    in if isJust next then
        (Just ('\\': fromJust next), nextRest)
    else
        (Just "\\", nextRest)
getCharacter (c : rest) = (Just [c], rest)

getAsStr:: String -> (Maybe String, String)
getAsStr "" = (Nothing, "")
getAsStr context = let
    (char, rest) = getCharacter context
    in if isJust char then
        if char == Just "\"" || char == Just "\"" then
            (Just "", rest)
        else let
            (nextStr, nextRest) = getAsStr rest 
        in if isJust nextStr then
            (Just (fromJust char ++ fromJust nextStr), nextRest)
        else (Nothing, context)
    else (Nothing, context)

getAsNum :: String -> (String, String)
getAsNum "" = ("", "")
getAsNum (c : rest) | isDigit c || c == '.' = let (nextNum, nextRest) = getAsNum rest
        in (c : nextNum, nextRest)
getAsNum context = ("", context)

getAsList:: String -> (Maybe [JsonValue], String)
getAsList context = (Nothing, context)
-- todo

getAsObject:: String -> (Maybe [NamedValue], String)
getAsObject context = (Nothing, context)
--todo

getItem:: String -> (Maybe JsonValue, String)
getItem context = let (maybeC, rest) = getCharacter context
    in if isJust maybeC then
        let c = fromJust maybeC
        in if c == "\"" then
            let (maybeStr, strRest) = getAsStr rest
            in if isJust maybeStr then
                (Just (JsonString (fromJust maybeStr)), strRest)
            else
                (Nothing, context)
        else if isDigit (head c) then
            let (numStr, numRest) = getAsNum context
            in (Just (JsonNumber (read numStr)), numRest)
        else if take 4 context == "null" then
            (Just JsonNull, drop 4 context)
        else if take 4 context == "true" then
            (Just (JsonBool True) , drop 4 context)
        else if take 5 context == "false" then
            (Just (JsonBool False)  , drop 5 context)
        else if c == " " || c == "\r" || c == "\n" then
            getItem rest
        else if c == "[" then
            let (maybeList, listRest) = getAsList rest
            in if isJust maybeList then
                (Just (JsonList (fromJust maybeList)), listRest)
            else (Nothing, context)
        else if c == "{" then
            let (maybeObject, objectRest) = getAsObject rest
            in if isJust maybeObject then
                (Just (JsonObject (fromJust maybeObject)), objectRest)
            else (Nothing, context)
        else (Nothing, context)
    else (Nothing, context)