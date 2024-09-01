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