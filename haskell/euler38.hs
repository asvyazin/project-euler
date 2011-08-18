import Euler.Math
import Data.Maybe

allDistinct [] = True
allDistinct (x:xs) = (allDistinct xs) && (not (elem x xs))

isPandigital n = allDistinct $ numberToDigits10 n

multiples n = map (n *) [1..]
multiplesList n = map numberToDigits10 $ multiples n
multiplesFold n = takeWhile shortEnough $ scanl1 (++) $ multiplesList n
  where shortEnough l = (length l) < 10
        
pandigital n
  | (len == 9) && (allDistinct l) = Just (digitsToNumber10 l)
  | otherwise = Nothing
  where
    l = last $ multiplesFold n
    len = length l
    
main = print $ maximum $ catMaybes $ map pandigital [1..9999]