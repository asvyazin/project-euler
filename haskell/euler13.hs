import Euler.Math

main = do
  file <- readFile "euler13.txt"
  let
    almostResult = sum $ map readInteger $ lines file
    readInteger = read :: String -> Integer    
    result = digitsToNumber10 $ take 10 $ numberToDigits10 almostResult
   in
   putStrLn $ show result
