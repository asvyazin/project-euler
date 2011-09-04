import Data.List

square n = n * n

isRightTriangle a b c = ((square a) + (square b)) == (square c)

numberOfSolutions p = length l
  where
    l = [1 | a <- [1..(p `div` 2)], 
         b <- [a..((p + a) `div` 2)], (isRightTriangle a b (p - a - b))]
    
myCompare (_, a) (_, b) = compare a b
    
main = print $ maximumBy myCompare solutionsList
  where
    solutionsList = map (\x -> (x, numberOfSolutions x)) [1..1000]