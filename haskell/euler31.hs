coins 0 _ = 1
coins _ [] = 0
coins n l@(x:xs)
  | x > n = coins n xs
  | otherwise = (coins (n - x) l) + (coins n xs)

main = print $ coins 200 [200, 100, 50, 20, 10, 5, 2, 1]