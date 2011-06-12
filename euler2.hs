next (x, y) = (x + y, x)

fib = map snd $ iterate next (1, 1)

euler2 x = sum $ filter even $ takeWhile (<= x) fib