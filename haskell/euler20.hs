import Euler.Math

factorial n = foldl1 (*) [1..n]
sumOfDigits = sum . numberToDigits10

main = print $ sumOfDigits $ factorial 100