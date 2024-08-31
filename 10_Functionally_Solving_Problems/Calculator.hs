module Calculator where

import Data.List
import Test.HUnit
import GHC.Generics (C)

{- Reverse Polish notation calculator -}
-- 10 4 3 + 2 * - = 10 - (4 + 3) * 2 = -4
-- RPN "10 4 3 + 2 * -" = -4

data Operator = Add | Subtract | Multiply deriving (Eq, Show)


class Read b => Expression b where
    toNumber :: b -> Double
    isOperator :: b -> Maybe Operator
instance Expression String where
    toNumber = read
    isOperator "+" = Just Add
    isOperator "-" = Just Subtract
    isOperator "*" = Just Multiply
    isOperator _   = Nothing
type ExpressionList b = [b]

class Num a => CalcItem a
instance CalcItem Int
type CalcStack a = [a]

foldingFunction :: (CalcItem a, Expression b) => CalcStack a -> b -> CalcStack a
foldingFunction (x:y:ys) op = case isOperator op of
    Just Multiply -> (x * y) : ys
    Just Add      -> (x + y) : ys
    Just Subtract -> (y - x) : ys
    Nothing       -> (fromIntegral (round (toNumber op))) : x : y : ys
foldingFunction xs numberString = (fromIntegral (round (toNumber numberString))) : xs


solveRPN :: CalcItem a => String -> a
solveRPN = head . foldl foldingFunction [] . words




{- Unit Tests -}                      
wordTest = TestCase (assertEqual "words" ["10","4","3","+","2","*","-"] (words "10 4 3 + 2 * -" ))
readTest = TestCase (assertEqual "read" 4 (read "4"))
rpnTest = TestCase (assertEqual "rpn" (-4 :: Int) (solveRPN "10 4 3 + 2 * -" ))
tests = TestList [wordTest,
                  readTest,
                  rpnTest]

unitTests :: IO Counts
unitTests = runTestTT tests

