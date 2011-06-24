import Euler.Math

takeFirst f =
  head . dropWhile (not . f)
triangles = scanl1 (+) [1..]
divisorsCount = length . divisorsUnsorted
main =
  let
    whatWeNeed n = (divisorsCount n) > 500
    res = takeFirst whatWeNeed triangles
  in
   putStrLn $ show res