module Euler.Math where

import Data.List

divideBy n x = (x `mod` n) == 0

numberToDigits n base =
  reverse $ numberToDigitsRev n base
  where
    numberToDigitsRev 0 _ = []
    numberToDigitsRev n base =
      q:(numberToDigitsRev p base)
      where
        (p, q) = divMod n base
        
numberToDigits10 = (`numberToDigits` 10)

isNumberPalindrome n base =
  isListPalindrome $ numberToDigits n base
  where
    isListPalindrome l = 
      l == (reverse l)
      
isNumberPalindrome10 = (`isNumberPalindrome` 10)

square n = n * n

isPrime n
  | n == 2 = True
  | n == 3 = True
  | even n = False
  | otherwise =
    all (\p -> not $ divideBy p n) testingSlice
    where
      testingNumbers = iterate (+ 2) 3
      testingSlice = takeWhile (\p -> square p <= n) testingNumbers
      
nextPrime n =
  let Just m = find isPrime [n + 1..]
  in m
     
allPrimes =
  iterate nextPrime 2
  
divisorsUnsorted n =
  concat $ map divisorClone divisorsSmall
  where isSmallEnough p = (square p) <= n
        isSqrt p = (square p) == n
        isDivisor p = divideBy p n
        divisorsSmall = filter isDivisor $ takeWhile isSmallEnough [1..]
        divisorClone p
          | isSqrt p = [p]
          | otherwise = [p, n `div` p]
