import Euler.Math
import Data.List
import qualified Data.Set as Set

numbersFromList l n m = 
  (digitsToNumber10 l1, digitsToNumber10 l2, digitsToNumber10 l3)
  where
    (l1, l') = splitAt n l
    (l2, l3) = splitAt m l'
    
isGood (a, b, c) = a * b == c
third (_, _, x) = x

main = print $ sum $ Set.toList goodProducts
  where
    pp = permutations [1, 2, 3, 4, 5, 6, 7, 8, 9]
    goodProducts = Set.fromList $ map third $ filter isGood 
                   [numbersFromList p n (5 - n)| p <- pp, n <- [1..4]]
