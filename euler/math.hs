module Euler.Math where

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