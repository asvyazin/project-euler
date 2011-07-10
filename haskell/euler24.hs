import qualified Data.List as L

main = print $ (L.sort $ L.permutations "0123456789") !! 999999