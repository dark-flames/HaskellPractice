module SpliteLines(spliteLines) where

isLineTerminator c = c == '\r' || c == '\n'

spliteLines :: String -> [String]
spliteLines [] = [];
spliteLines str = 
    let (line, suffix) = break isLineTerminator str
    in line : case suffix of
        ('\r' : '\n' : rest) -> spliteLines rest 
        ('\n' : rest)        -> spliteLines rest
        ('\r' : rest)        -> spliteLines rest
        _                    -> []

