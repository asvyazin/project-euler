import Euler.Math

maxPrimeFactor n =
  maxPrimeFactor n 2 where
    maxPrimeFactor n p
      | n == p = p
      | divideBy p n = maxPrimeFactor (n `div` p) p
      | otherwise = maxPrimeFactor n (p + 1)
               