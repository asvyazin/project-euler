import Euler.Math
import Data.Ratio
import Data.Maybe

dig2 a b = 10 * a + b

isGood1 a b c
  | b == c = False
  | otherwise = ((dig2 a b) % (dig2 a c)) == (b % c)

isGood2 a b c
  | b == c = False
  | otherwise = ((dig2 a b) % (dig2 c a)) == (b % c)

isGood3 a b c
  | a == c = False
  | otherwise = ((dig2 a b) % (dig2 b c)) == (a % c)

isGood4 a b c
  | a == c = False
  | otherwise = ((dig2 a b) % (dig2 c b)) == (a % c)

goodRatio a b c
  | isGood1 a b c = Just (b % c)
  | isGood2 a b c = Just (b % c)
  | isGood3 a b c = Just (a % c)
  | isGood4 a b c = Just (a % c)
  | otherwise = Nothing
                
main = print $ denominator $ product $ 
       filter (< 1) $ 
       map fromJust $ 
       filter isJust
       [goodRatio a b c | a <- [1..9], b <- [1..9], c <- [1..9]]