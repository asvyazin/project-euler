numStr_1 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 
            "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", 
            "seventeen", "eighteen", "nineteen"]
numStr_2 = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", 
            "eighty", "ninety"]

numberToText n =
  let
    d100 = n `div` 100
    s100
      | d100 > 0 = (numStr_1 !! d100) ++ "hundred"
      | otherwise = ""
    n100 = n `mod` 100
    a
      | d100 > 0 = "and"
      | otherwise = ""
    s1
      | n100 == 0 = ""
      | n100 < 20 = a ++ numStr_1 !! n100
      | otherwise = a ++
        let
          d10 = n100 `div` 10
          s10 = numStr_2 !! d10
          n1 = n100 `mod` 10
        in
         s10 ++ (numStr_1 !! n1)
  in
   s100 ++ s1
   
main =
  let
    s1_999 = sum $ map length $ map numberToText [1..999]
  in
   print $ s1_999 + 11