import Euler.Math

main =
  let
    n = 2 ^ 1000
    digits = numberToDigits10 n
  in
   print $ sum digits