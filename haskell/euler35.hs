import Euler.Math

listRotations l = map combinePair pairs
  where
    len = length l
    pairs = map ($ l) $ map splitAt [0..(len - 1)]
    combinePair (l1, l2) = l2 ++ l1
    
numberRotations = map digitsToNumber10 . listRotations . numberToDigits10
isCircular = all isPrime . numberRotations

main = print $ length $ filter isCircular [2..999999]