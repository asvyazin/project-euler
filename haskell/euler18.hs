combineLists l1 l2 =
  let
    l1_1 = l1 ++ [0]
    m1 = zipWith (+) l1_1 l2
    l1_2 = [0] ++ l1
    m2 = zipWith (+) l1_2 l2
  in
   zipWith max m1 m2
   
main = do
  file <- readFile "euler18.txt"
  let
    l = lines file
    readInteger = read :: String -> Integer
    table = map (map readInteger . words) l
    resultTable = foldl1 combineLists table
    in
   print $ maximum resultTable