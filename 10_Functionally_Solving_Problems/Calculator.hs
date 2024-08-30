module Calculator where

import Data.List
import Test.HUnit

--Reverse Polish notation calculator


solveRPN :: (Num a, Read a) => String -> a
solveRPN = undefined

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2

                        
test1 = TestCase (assertEqual "for (doubleSmallNumber 101)," 101 (doubleSmallNumber 101))
test2 = TestCase (assertEqual "for (doubleSmallNumber 50)," 100 (doubleSmallNumber 50))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

