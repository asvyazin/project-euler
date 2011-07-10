import Euler.Math
import qualified Data.List as L

isAbundant n = sumOfProperDivisors n > n
goodNums nn = filter isGood [1..nn]
  where 
    abundants = filter isAbundant [1..nn]
    isAbundant2 n = elem n abundants
    isGood n = all (not . f) [1..n2]
      where
        n2 = n `div` 2
        f m = (isAbundant2 m) && (isAbundant2 (n - m))
main = print $ sum $ goodNums 28122