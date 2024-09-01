module Accumulator where

-- Importing traceShow for debugging purposes
import Debug.Trace (traceShow)
import Data.Ratio ((%))

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ z []     = z
foldRight f z (x:xs) = f x (foldRight f z xs)

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ z []     = z
foldLeft f z (x:xs) = foldLeft f (f z x) xs

-- foldr example: right fold
foldrExample :: (Show a, Show b) => (a -> b -> b) -> b -> [a] -> b
foldrExample f = foldRight (\x acc' -> traceShow ("foldr", x, acc') (f x acc'))

-- foldl example: left fold
foldlExample :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
foldlExample f = foldLeft (\acc' x -> traceShow ("foldl", acc', x) (f acc' x))

-- Example functions to use with foldr and foldl
divideFunction :: Rational -> Rational -> Rational
divideFunction = (/)

-- Main function to demonstrate the usage
main :: IO ()
main = do
    let list = [1, 2, 3]
    let rationalList = map (% 1) list  -- Convert list of integers to list of Rationals
 
    putStrLn "Using foldr to divide elements [1, 2, 3], using 1 as the initial value:"
    putStrLn "(step, x, acc')"
    let foldrResult = foldrExample divideFunction 1 rationalList
    print foldrResult

    putStrLn "\nUsing foldl to divide elements [1, 2, 3], using 1 as the initial value:"
    putStrLn "(step, x, acc')"
    let foldlResult = foldlExample divideFunction 1 rationalList
    print foldlResult
{- 
ghci> main
Using foldr to divide elements [1, 2, 3], using 1 as the initial value:
(step, x, acc')
("foldr",3 % 1,1 % 1)
("foldr",2 % 1,3 % 1)
("foldr",1 % 1,2 % 3)
3 % 2

Using foldl to divide elements [1, 2, 3], using 1 as the initial value:
(step, x, acc')
("foldl",1 % 1,1 % 1)
("foldl",1 % 1,2 % 1)
("foldl",1 % 2,3 % 1)
1 % 6 -}

-- Let us analyze foldRight and foldLeft with the divideFunction applied
-- to the list [1, 2, 3] and the initial value 1:

-- foldRight (/) 1 [1, 2, 3] =
-- foldRight (/) 1 1:[2, 3] =
-- (/) 1 (foldRight (/) 1 [2, 3]) =
-- (/) 1 (foldRight (/) 1 2:[3]) =
-- (/) 1 ((/) 2 (foldRight (/) 1 [3])) =
-- (/) 1 ((/) 2 ((/) 3 (foldRight (/) 1 []))) =
-- (/) 1 ((/) 2 ((/) 3 1)) =
-- (/) 1 ((/) 2 3) =
-- (/) 1 (2 % 3) =
-- 3 % 2

-- foldLeft (/) 1 [1, 2, 3] =
-- foldLeft (/) 1 1:[2, 3] =
-- foldLeft (/) ((/) 1 1) [2, 3] =
-- foldLeft (/) (1 % 1) [2, 3] =
-- foldLeft (/) (1 % 1) 2:[3] =
-- foldLeft (/) ((/) (1 % 1) 2) [3] =
-- foldLeft (/) (1 % 2) [3] =  
-- foldLeft (/) ((/) (1 % 2) 3) [] =
-- 1 % 6
