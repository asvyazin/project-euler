import Euler.Math

divideBy3or5 x = (divideBy 3 x) || (divideBy 5 x)

euler1 x = sum $ filter divideBy3or5 [1..(x - 1)]