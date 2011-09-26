modulo = 10000000000
powMod n m = (n ^ m) `mod` modulo
plusMod n m = (n + m) `mod` modulo
powSelf n = powMod n n
main = print $ foldl1 plusMod $ map powSelf [1..1000]