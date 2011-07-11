import Data.List
import Data.Maybe

cycleLength (x:xs) = elemIndex x xs

next n l@(p:ps) = ((10 * p) `mod` n):l

ratios n = iterate (next n) [1]

ratio n = (length r) - 1
  where 
    r:_ = dropWhile (not . isJust . cycleLength) $ ratios n
        
main = print (result + 1)
  where
    rats = map ratio [1..1000]
    m = maximum rats
    Just result = elemIndex m rats