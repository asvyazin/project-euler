import Data.List

next p
  | even p = p `div` 2
  | otherwise = 3 * p + 1
                        
collatzChain n =
  iter [n]
  where iter a@(1:_) = a
        iter a@(p:_) = iter $ (next p):a
                        
collatzLength' = length . collatzChain
collatzLength n = 
  iter n 1
  where
    iter 1 p = p
    iter q p = iter (next q) (p + 1)

main =
  let
    comparePairs (n, m) (n', m') = compare m m'    
    (res, len) = maximumBy comparePairs $ map (\n -> (n, collatzLength n)) [1..1000000]
  in
   putStrLn $ show res ++ " " ++ show len
