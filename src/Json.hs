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

data JsonToken = Str String
    | SquareBracket [JsonToken]
    | CurlyBracket [JsonToken]
    | Lit String


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
