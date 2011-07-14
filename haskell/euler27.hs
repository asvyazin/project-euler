import Euler.Math
import Data.List

quadratic a b n = (a + n) * n + b
primesAB a b = length $ takeWhile isPrime $ map (quadratic a b) [0..]
primesCompare (_, _, z1) (_, _, z2) = compare z1 z2

main = print $ a * b
  where
    (a, b, _) = maximumBy primesCompare 
                [(x, y, primesAB x y) | x <- [-999..999], y <- [2..999], isPrime y]