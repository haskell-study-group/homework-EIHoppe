--5.1
sum [x^2 | x <- [1..100]]

--5.2
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

--5.3
square n = [(x, y) | (x, y) <- grid n n, x /= y]

--5.4
replicate n x = [x | _ <- [1..n]]

--5.5
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--5.6
--(with factors taken from the book example)
perfects n = [x | x <- [1..n], x == sum (init (factors x))]

--5.7
--????

--5.8
positions x xs = [i | i <- find x (zip xs [0..])]

--5.9
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

--5.10
import Data.Char

let2int base c = ord c - ord base

int2let base n = chr (ord base + n)

shift n c | isLower c = int2let 'a' ((let2int 'a' c + n) `mod` 26)
          | isUpper c = int2let 'A' ((let2int 'A' c + n) `mod` 26)
          | otherwise = c

encode n xs = [shift n x | x <- xs]