import Euler.Math

isTruncatable n = isTruncatableLeft && isTruncatableRight && (len > 1)
  where
    digits = numberToDigits10 n
    len = length digits
    truncatesList [] = []
    truncatesList a@(_:xs) = a:(truncatesList xs)
    isTruncatableLeft = all isPrime truncates
      where
        truncates = map digitsToNumber10 $ truncatesList $ digits
    isTruncatableRight = all isPrime truncates
      where
        truncates = map (digitsToNumber10 . reverse) $ truncatesList $ reverse $ digits
        
main = print $ sum $ take 11 $ filter isTruncatable [1..]