module Option(Option(Some, None), isSome, optionMap) where

data Option a = Some a | None deriving(Eq, Show)

isSome :: Option a -> Bool
isSome (Some _)  = True 
isSome (None) = False

optionMap:: Option a -> (a -> b) -> Option b
optionMap (Some nested) func = Some (func nested)
optionMap (None) _ = None