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

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0


