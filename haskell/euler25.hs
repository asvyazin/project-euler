import Euler.Math

numberLength = length . numberToDigits10
main = print (index + 1)
  where
    isGood n = (numberLength n) >= 1000
    index = length $ takeWhile (not . isGood) fibonacci