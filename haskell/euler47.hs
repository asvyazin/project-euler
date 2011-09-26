import Euler.Math

nums = [647..]
nums2 = map (length . distinctPrimeFactors) nums

findNum :: [(Int, Int)] -> Int
findNum ((x, 4) : (_, 4) : (_, 4) : (_, 4) : _) = x
findNum (_:xs) = findNum xs

main = print $ findNum $ zip nums nums2