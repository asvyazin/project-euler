import Data.List

next n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzLength = length . takeWhile (/= 1) . iterate next
                
main = print $ maximum $ map (\n-> (collatzLength n, n)) [500001..999999]
