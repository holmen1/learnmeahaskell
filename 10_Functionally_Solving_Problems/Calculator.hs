module Calculator where

import Data.List
import Test.HUnit
import GHC.Generics (C)

{- Reverse Polish notation calculator -}
-- 10 4 3 + 2 * - = 10 - (4 + 3) * 2 = -4
-- RPN "10 4 3 + 2 * -" = -4

type ExpressionList = [String]

foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
foldingFunction (x:y:ys) "*" = (x*y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs numberString = read numberString:xs

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words




{- Unit Tests -}                      
wordTest = TestCase (assertEqual "words" ["10","4","3","+","2","*","-"] (words "10 4 3 + 2 * -" ))
readTest = TestCase (assertEqual "read" 4 (read "4"))
rpnTest = TestCase (assertEqual "rpn" (-4) (solveRPN "10 4 3 + 2 * -" ))
tests = TestList [wordTest,
                  readTest]--,
--                  rpnTest]

unitTests :: IO Counts
unitTests = runTestTT tests

