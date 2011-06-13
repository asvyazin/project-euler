import Euler.Math

numbers = [100..999]
testingNumbers = [x * y | x <- numbers, y <- numbers]

euler4 =
  maximum $ filter isNumberPalindrome10 testingNumbers