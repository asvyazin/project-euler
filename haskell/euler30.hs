import Euler.Math

isGood n = sum5 == n
  where sum5 = sum $ map (^ 5) $ numberToDigits10 n
        
main = print $ sum $ filter isGood [10..354294]