import qualified Data.Text as T
import qualified Data.List as L
import Data.Char

charValue c = (ord c) - (ord 'A') + 1
wordValue = sum . map charValue . T.unpack

triangles = map triangleN [1..]
  where triangleN n = (n * (n + 1)) `div` 2
        
isTriangle n = (m == n)
  where m = last $ takeWhile (<= n) triangles

main = do
  file <- readFile "../problem42.txt"
  let
    delim = T.pack  ","
    trim = T.dropAround (== '"')
    words = L.sort $ map trim $ T.splitOn delim $ T.pack file
    wordValues = map wordValue words
    in
   print $ length $ filter isTriangle wordValues