import Euler.Math

twiceSquares = map (\n -> 2 * (square n)) [1..]
isGood n = any isPrime [n - sq | sq <- takeWhile (< n) twiceSquares]

main = print $ 
       head $ 
       filter (not . isGood) $ 
       filter (not . isPrime) $
       map (\n-> 2 * n + 1) [1..]