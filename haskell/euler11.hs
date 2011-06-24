import Data.List

slices :: [a] -> [[a]]
slices (x1:x2:x3:x4:xs) = [x1, x2, x3, x4]:slices (x2:x3:x4:xs)
slices _ = []

safeMax :: [Integer] -> Integer
safeMax [] = 0
safeMax x = maximum x

maxSlice :: [Integer] -> Integer
maxSlice =
  safeMax . map multAll . slices
  where multAll = foldl1 (*)
        
isEmpty [] = True
isEmpty _ = False

listify x = [x]

halfTranspose [] = [[]]
halfTranspose (l:ls) =
  [x]:(transpose [xs, halfTranspose ls])
  where (x:xs) = l

main = do
  file <- readFile "euler11.txt"
  let
    lineToNumbers = map (read :: String -> Integer) . words
    tableToMax = safeMax . map maxSlice
    table = map lineToNumbers $ lines file
    table' = transpose table
    m1 = tableToMax table
    m2 = tableToMax table'
    m = maximum [m1, m2]
    in
   putStrLn $ show m