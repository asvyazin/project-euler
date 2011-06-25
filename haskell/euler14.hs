import Data.List

next p
  | even p = p `div` 2
  | otherwise = 3 * p + 1
    
collatzLength 1 = 1
collatzLength n =
  1 + (collatzLength $ next n)

main =
  let
    comparePairs (n, m) (n', m') = compare m m'    
    (res, len) = maximumBy comparePairs $ map (\n -> (n, collatzLength n)) [1..1000000]
  in
   putStrLn $ show res ++ " " ++ show len
