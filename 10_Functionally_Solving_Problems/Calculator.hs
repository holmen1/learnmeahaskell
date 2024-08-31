module Calculator where

import Data.List
import Test.HUnit
import Text.ParserCombinators.ReadP (string)

{- Reverse Polish notation calculator -}
-- 10 4 3 + 2 * - = 10 - (4 + 3) * 2 = -4
-- RPN "10 4 3 + 2 * -" = -4

type Expression = String
type ExpressionList = [Expression]

type CalcStack = [Float]

stringToExpressionList :: String -> ExpressionList
stringToExpressionList = words

foldingFunction :: CalcStack -> Expression -> CalcStack
foldingFunction (x:y:ys) "*" = (x * y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs numberString = read numberString:xs

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . stringToExpressionList


{- Unit Tests -}                      
wordTest = TestCase (assertEqual "words" ["10","4","3","+","2","*","-"] (words "10 4 3 + 2 * -" ))
readTest = TestCase (assertEqual "read" 4 (read "4"))
rpnTest = TestCase (assertEqual "rpn" (-4.0) (solveRPN "10 4 3 + 2 * -"))

tests = TestList [wordTest,
                  readTest,
                  rpnTest]


