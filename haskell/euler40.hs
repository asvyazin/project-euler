import Euler.Math

main = print $ mult
  where
    mult = (d 1) * (d 10) * (d 100) * (d 1000) * (d 10000) *
           (d 100000) * (d 1000000)
    d n = irrational !! (n - 1)
    irrational = concat $ map numberToDigits10 [1..]