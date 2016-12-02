--6.1
--it infinite loops/stack overflows
fac 0         = 1
fac n | n > 0 = n * fac (n - 1)

--6.2
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

--6.3
(^) :: Int -> Int -> Int
m ^ 0         = 1
m ^ n | n > 0 = m * (m ^ (n-1))

--sample calculation:
--4^3
--4*(4^2)
--4*4*(4^1)
--4*4*4*(4^0)
--4*4*4*1
--64

--6.4
euclid x y | x == y = x
euclid x y | x > y  = euclid (x-y) y
euclid x y | x < y  = euclid x (y-x)

--6.5a
{-
length [1,2,3]
1 + length [2,3]
1 + 1 + length[3]
1 + 1 + 1 + length[]
1 + 1 + 1 + 0
3
-}

--6.5b
{-
drop 3 [1,2,3,4,5]
drop 2 [2,3,4,5]
drop 1 [3,4,5]
drop 0 [4,5]
[4,5]
-}

--6.5c
{-
init [1,2,3]
[1] : init [2,3]
[1] : [2] : init [3]
[1] : [2] : []
[1,2]
-}

--6.6a
and [x]    = x
and (x:xs) = x && (and xs)

--6.6b
--not sure how to get this to work syntax-wise, moving on for now
--general idea is pluck the first list off and append to concat remaining list of lists
{-
concat [[]]         = []
concat [(xs : xss)] = xs : (concat xss)
-}

--6.6c
replicate 0 x = []
replicate n x = x : replicate (n - 1) x

--6.6d
(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! n = xs !! (n - 1)

--6.6e
elem f []                 = False
elem f (x:xs) | f == x    = True
              | otherwise = elem f xs

--6.7
merge [] []                  = []
merge xs []                  = xs
merge [] ys                  = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y : ys)
                    | x >  y = y : merge (x : xs) ys

--6.8
msort []     = []
msort (x:[]) = [x]
msort xs     = merge (msort (take n xs)) (msort (drop n xs))
               where n = length xs `div` 2

--6.9a
--step 1
sum :: [Num] -> Num

--step 2
{-
sum []
sum (x:xs)
-}

--step 3
{-
sum []     = 0
sum (x:xs) =
-}

--step 4
sum []     = 0
sum (x:xs) = x + sum xs

--step 5
sum = foldr (+) 0

--6.9b
--step 1
take :: Int -> [a] -> [a]

--step 2
{-
take 0 []
take 0 xs
take n xs
-}

--step 3
{-
take 0 [] = []
take 0 xs = []
take n xs =
-}

--step 4
take 0 []     = []
take 0 xs     = []
take n (x:xs) = x : take (n-1) xs

--step 5
take 0 _      = []
take n (x:xs) = x : take (n-1) xs

--6.9c
--step 1
last :: [a] -> a

--step 2
{-
last [x]
last xs
-}

--step 3
{-
last [x]    = x
last (x:xs) =
-}

--step 4
last [x]    = x
last (x:xs) = last xs

--step 5
last [x]    = x
last (_:xs) = last xs