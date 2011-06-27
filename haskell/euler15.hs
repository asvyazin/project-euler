main = let
  mult = foldl1 (*)
  n1 = mult [21..40]
  n2 = mult [1..20]
  in
   print $ n1 `div` n2