import Euler.Math

properDivisors n = filter (/= n) $ divisorsUnsorted n
sumOfProperDivisors = sum . properDivisors
isAmicable n
  | m == n = False
  | sumOfProperDivisors m == n = True 
  | otherwise = False
  where
    m = sumOfProperDivisors n

main = print $ sum $ filter isAmicable [1..9999]