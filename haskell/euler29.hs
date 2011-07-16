import qualified Data.Set as Set

main = print $ Set.size $ Set.fromList [x ^ y | x <- [2..100], y <- [2..100]]