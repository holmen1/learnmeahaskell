-- By calling functions with too few parameters, so to speak, we're creating new functions on the fly.
-- What if we wanted to create a function that takes a number and compares it to 100?
-- We could do something like this:
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x

-- If we call it with 99, it returns a GT. Simple stuff. Notice that the x is on the right
-- hand side on both sides of the equation. Now let's think about what compare 100 returns.
-- It returns a function that takes a number and compares it with 100.
-- Wow! Isn't that the function we wanted? We can rewrite this as:
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100
-- The type declaration stays the same, because compare 100 returns a function. Compare has a type of (Ord a) => a -> (a -> Ordering) and calling it with 100 returns a (Num a, Ord a) => a -> Ordering. The additional class constraint sneaks up there because 100 is also part of the Num typeclass.

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

-- Let's find the largest number under 100,000 that's divisible by 3829. To do that,
-- we'll just filter a set of possibilities in which we know the solution lies.
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0
-- ghci> largestDivisible
-- 99554

--Let's implement sum again, only this time, we'll use a fold instead of explicit recursion.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- ghci> sum' [3,5,2,1]
-- 11

-- If we take into account that functions are curried, we can write this implementation ever ore
-- succinctly, like so:
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states
-- in the form of a list.
-- ghci> scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]
-- ghci> scanr (+) 0 [3,5,2,1]
-- [11,8,3,1,0]

