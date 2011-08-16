import Euler.Math

main = print $ sum $ filter isGood [1..999999]
  where
    isGood n = (isNumberPalindrome 2 n) && (isNumberPalindrome 10 n)