import qualified Data.Text as T
import Data.Char
import qualified Data.Sequence as S
import qualified Data.List as L

charValue c = (ord c) - (ord 'A' ) + 1
nameValue = sum . map charValue . T.unpack

main = do
  file <- readFile "names.txt"
  let
    delim = T.pack ","
    trim = T.dropAround (== '"')
    names = L.sort $ map trim $ T.splitOn delim $ T.pack file
    nameValues = map nameValue names
    valuesSeq = S.fromList nameValues
    result = S.foldlWithIndex (\s -> \i -> \x -> s + x * (i + 1)) 0 valuesSeq
    in
   print $ result