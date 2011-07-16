import Euler.Math

factorial 0 = 1
factorial n = product [1..n]
maxN = 7 * (factorial 9) - 1

isGood n = s == n
  where
    s = sum $ map factorial $ numberToDigits10 n
    
main = print $ sum $ filter isGood [3..maxN]