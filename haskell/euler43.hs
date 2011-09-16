import Euler.Math
import Data.List

isGoodNumber [_, d2,d3, d4, d5, d6, d7, d8, d9, d10] =
  (divideBy 2 $ digitsToNumber10 [d2, d3, d4]) &&
  (divideBy 3 $ digitsToNumber10 [d3, d4, d5]) &&
  (divideBy 5 $ digitsToNumber10 [d4, d5, d6]) &&
  (divideBy 7 $ digitsToNumber10 [d5, d6, d7]) &&
  (divideBy 11 $ digitsToNumber10 [d6, d7, d8]) &&
  (divideBy 13 $ digitsToNumber10 [d7, d8, d9]) &&
  (divideBy 17 $ digitsToNumber10 [d8, d9, d10])
  
main = print $ 
       sum $ 
       map digitsToNumber10 $ 
       filter isGoodNumber $ 
       permutations [0..9]
