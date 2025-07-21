-- import Data.Sequence (Seq(Empty))
data IntList
    = Empty
    | Cons Int IntList

lengthIntList :: IntList -> Int
lengthIntList Empty = 0
lengthIntList (Cons _ xs) = 1 + lengthIntList xs

showInt :: Int -> String
showInt n = show n