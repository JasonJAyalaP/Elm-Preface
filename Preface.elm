module Preface where

isEmpty : [a] -> Bool
isEmpty xs = case xs of
               []     -> True
               (_::_) -> False

repeat : a -> Int -> [a]
repeat x n = if | n <= 0    -> []
                | otherwise -> x::repeat x (n-1)

replicate : Int -> a -> [a]
replicate n x = repeat x n

isEven : Int -> Bool
isEven n = n `mod` 2 == 0

isOdd : Int -> Bool
isOdd = not . isEven

takeWhile : (a -> Bool) -> [a] -> [a]
takeWhile p xs = case xs of
                   []     -> []
                   (h::t) -> if | p h       -> h :: takeWhile p t
                                | otherwise -> []

dropWhile : (a -> Bool) -> [a] -> [a]
dropWhile p xs = case xs of
                   []     -> []
                   (h::t) -> if | p h       -> dropWhile p t
                                | otherwise -> xs

init : [a] -> [a]
init xs = case xs of
            []     -> []
            [x]    -> []
            (h::t) -> h :: init t

tail : [a] -> [a]
tail xs = case xs of
            []     -> []
            (h::t) -> t

(#) : [a] -> Int -> Maybe a
xs # n = case xs of
           []     -> Nothing
           (h::t) -> if | n <= 0    -> Nothing
                        | n == 1    -> Just h
                        | otherwise -> t # (n-1)

iterate : (a -> a) -> a -> Int -> [a]
iterate f x n = if | n <= 0    -> []
                   | n == 1    -> [x]
                   | otherwise -> x :: iterate f (f x) (n-1)

cycle : [a] -> Int -> [a]
cycle xs n = if | n <= 0    -> []
                | n == 1    -> xs
                | otherwise -> xs ++ cycle xs (n-1)

find : (a -> Bool) -> [a] -> Maybe a
find p xs = case xs of
              []   -> Nothing
              h::t -> if | p h       -> Just h
                         | otherwise -> find p t

intercalate : [a] -> [[a]] -> [a]
intercalate xs xss = xss |> intersperse xs |> concat

unfoldr : (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a :: unfoldr f b'
                Nothing      -> []
