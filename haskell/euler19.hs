import Euler.Math

days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapDays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

isLeapYear y
  | divideBy 400 y = True
  | divideBy 100 y = False
  | divideBy 4 y = True
  | otherwise = False
                
yearToDays y
  | isLeapYear y = leapDays
  | otherwise = days
allMonths = concat $ map yearToDays [1901..2000]
                
nextDay d m = (d + m) `mod` 7

allDays = scanl nextDay 2 allMonths

main = print $ length $ filter (== 0) allDays