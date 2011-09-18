import qualified Data.Set as S

indexLimit = 80000

triangleN n = (n * (n + 1)) `div` 2
trianglesFromIndex i = map triangleN [i..indexLimit]

pentagonalN n = (n * (3 * n - 1)) `div` 2
pentagonals = map pentagonalN [1..indexLimit]
pentagonalsSet = S.fromList pentagonals
isPentagonal p = S.member p pentagonalsSet

hexagonalN n = n * (2 * n - 1)
hexagonals = map hexagonalN [1..indexLimit]
hexagonalsSet = S.fromList hexagonals
isHexagonal p = S.member p hexagonalsSet

main = print $ head solutions
  where
    solutions = [t | t <- trianglesFromIndex 286, isPentagonal t, isHexagonal t]