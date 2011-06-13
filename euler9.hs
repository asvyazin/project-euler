import Euler.Math

isPythagorean (a, b, c) =
  square c == square a + square b
  
euler9 =
  let
    triplets = [(a, b, 1000 - a - b) | a <- [1..333], b <- [a..(666-a)]]
    [(a, b, c)] = filter isPythagorean triplets
    in a * b * c