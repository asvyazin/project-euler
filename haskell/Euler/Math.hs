module Euler.Math where

import Data.List

next (x, y) = (x + y, x)

fibonacci = map snd $ iterate next (1, 1)

divideBy n x = (x `mod` n) == 0

numberToDigits base n =
  reverse $ numberToDigitsRev n base
  where
    numberToDigitsRev 0 _ = []
    numberToDigitsRev n base =
      q:(numberToDigitsRev p base)
      where
        (p, q) = divMod n base
        
numberToDigits10 = numberToDigits 10

digitsToNumber base l =
  foldl1 (\n -> \d-> base * n + d) l
  
digitsToNumber10 = digitsToNumber 10

isNumberPalindrome base n =
  isListPalindrome $ numberToDigits base n
  where
    isListPalindrome l = 
      l == (reverse l)
      
isNumberPalindrome10 = isNumberPalindrome 10

square n = n * n

isPrime n
  | n <= 1 = False
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

properDivisors n = filter (/= n) $ divisorsUnsorted n
sumOfProperDivisors = sum . properDivisors

primeFactors n = iter 2 n
  where
    iter _ 1 = []
    iter p n
      | divideBy p n = p:(iter p (n `div` p))
      | otherwise = iter (p + 1) n
distinctPrimeFactors n = iter 2 False n
  where
    iter _ _ 1 = []
    iter p f n
      | divideBy p n =
        let factors = iter p True (n `div` p)
        in
         case f of
           True -> factors
           _ -> p:factors
      | otherwise = iter (p + 1) False n