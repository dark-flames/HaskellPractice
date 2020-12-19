module Json(JsonValue(JsonObject, JsonList, JsonString, JsonNumber, JsonBool, JsonNull)) where

import List
data JsonValue = JsonObject [(String, JsonValue)] 
    | JsonList [JsonValue]
    | JsonString String
    | JsonNumber Double
    | JsonBool Bool
    | JsonNull

showPair :: (String, JsonValue) -> String
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
