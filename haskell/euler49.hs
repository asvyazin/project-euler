import Euler.Math
import Data.List

listEq l1 l2 = (sort l1) == (sort l2)
isPermutations n1 n2 = listEq (numberToDigits10 n1) (numberToDigits10 n2)
concatNumbers = digitsToNumber10 . concat . map numberToDigits10
main = print [concatNumbers [n1, n1 + p, n1 + 2 * p] | 
              n1 <- filter isPrime [1000..9999], 
              p <- takeWhile (\x -> 2 * x <= 10000 - n1) [1..],
              isPrime (n1 + p), 
              isPrime (n1 + 2 * p),
              isPermutations n1 (n1 + p),
              isPermutations n1 (n1 + 2 * p)]