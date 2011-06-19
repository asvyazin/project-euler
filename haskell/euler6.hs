euler6 =
  squareOfSum - sumOfSquares
  where
    numbers = [1..100]
    square n = n * n
    sumOfSquares = sum $ map square numbers
    squareOfSum = square $ sum numbers