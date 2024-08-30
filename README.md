# Haskell Learning Project

## Description
This project contains various Haskell functions and examples as I actively read and learn from the book "Learn You a Haskell for Great Good!" by Miran Lipovača. The code snippets and exercises are implemented to reinforce the concepts covered in the book.

## Usage
To run the Haskell code, you can use GHC (Glasgow Haskell Compiler) or GHCi (the interactive environment for GHC).

### Example
To compile and run a Haskell file:
```sh
ghc -o outputFileName curry.hs
./outputFileName
```
To load and test functions in GHCi:
```sh
ghci curry.hs
```
### HUnit
```sh
cabal install --lib HUnit
```

```sh
ghci  
:load Calculator.hs  
ghci> runTestTT tests  
Cases: 2  Tried: 2  Errors: 0  Failures: 0  
Counts {cases = 2, tried = 2, errors = 0, failures = 0}  
```




## Reference
This project is based on the book "Learn You a Haskell for Great Good!" by Miran Lipovača:
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)  
The whole thing is completely free to read online, but it's also available in print and I encourage you to buy as many copies as you can afford! 

### License
This project is for educational purposes and follows the guidelines provided in the book "Learn You a Haskell for Great Good!".