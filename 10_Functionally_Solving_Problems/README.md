# Calculator Module

This Haskell module implements a Reverse Polish Notation (RPN) calculator. RPN is a mathematical notation in which every operator follows all of its operands, eliminating the need for parentheses to define the order of operations.

## Features

- Supports basic arithmetic operations: addition (`+`), subtraction (`-`), and multiplication (`*`).
- Interactive command-line interface for entering RPN expressions.
- Unit tests to verify the correctness of the implementation.

## Usage

### Running the Calculator

To run the calculator, load the module in GHCi and call the `main` function:

```sh
ghci Calculator.hs
ghci> main
```
You will be prompted to enter an RPN expression. The calculator will evaluate the expression and display the result. To exit the calculator press Enter without typing anything.

Example: 10 - ((4 + 3) * 2)
```sh
ghci> main
RPN Calculator
RPN>> 10 4 3 + 2 * -
-4.0
RPN>>
```

## Implementation Details
### Types
* `Expression`: A type alias for `String`, representing a single element in the RPN expression.  
* `ExpressionList`: A type alias for [Expression], representing a list of expressions.
* `CalcStack`: A type alias for [Float], representing the stack used for calculations.
### Functions
* `stringToExpressionList :: String -> ExpressionList`: Converts a string representing an RPN expression into a list of expressions.
* `foldingFunction :: CalcStack -> Expression -> CalcStack`: Processes an expression and updates the calculation stack.
* `solveRPN :: String -> Float`: Evaluates an RPN expression and returns the result.
### Unit Tests
The module includes unit tests to verify the correctness of the functions. The tests are defined using the `Test.HUnit library`.

To run the tests, load the module in GHCi and execute the runTestTT function with the tests list:
```sh
ghci Calculator.hs
ghci> runTestTT tests
Cases: 3  Tried: 3  Errors: 0  Failures: 0
Counts {cases = 3, tried = 3, errors = 0, failures = 0}
```

## PS
foldr (fold right)
foldr processes the list from right to left. It takes three arguments:

A binary function f of type (a -> b -> b).
An initial accumulator value z of type b.
A list [x1, x2, ..., xn] of type [a].
Mathematically, foldr f z [x1, x2, ..., xn] is defined as:

foldr f z [x1, x2, ..., xn] = x1 f (x2  f (... (xn f z) ...))

For example, if f is the division operator / and the list is [1, 2, 3] with an initial value 1, then:

foldr (/) 1 [1, 2, 3] = 1 / (2 / (3 / 1))

foldl (fold left)
foldl processes the list from left to right. It takes three arguments:

A binary function f of type (b -> a -> b).
An initial accumulator value z of type b.
A list [x1, x2, ..., xn] of type [a].
Mathematically, foldl f z [x1, x2, ..., xn] is defined as:

foldl f z [x1, x2, ..., xn] = (...((z  f  x1) f x2) f ...) f xn 

For example, if f is the division operator / and the list is [1, 2, 3] with an initial value 1, then:

foldl (/) 1 [1, 2, 3] = ((1 / 1) / 2) / 3 

Summary
foldr starts folding from the rightmost element of the list and applies the function f recursively.
foldl starts folding from the leftmost element of the list and applies the function f iteratively.