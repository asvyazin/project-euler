import Euler.Math
import Data.List

pandigitalPrimes n = filter isPrime $
                     map digitsToNumber10 $
                     permutations [1..n]
main = print $ maximum $ concat $ map pandigitalPrimes [4..9]