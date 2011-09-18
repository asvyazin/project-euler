import qualified Data.Set as S

main = print $ head goodPentagonals
  where
    goodPentagonals = [p - q | p <- pentagonals, 
                       q <- takeWhile (< p) pentagonals,
                       isPentagonalSet $ p - q,
                       isPentagonalSet $ p + q]
      where
        pentagonalN n = (n * (3 * n - 1)) `div` 2
        pentagonals = map pentagonalN [1..5000]
        pentagonalsSet = S.fromList pentagonals
        isPentagonalSet p = S.member p pentagonalsSet
