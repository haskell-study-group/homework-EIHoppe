--7.1
--map f (filter p xs)

--7.2a
all p = and . map p
--7.2b
any p = or . map p
--7.2c
takeWhile _ []     = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                   | otherwise = []

--7.2d
dropWhile _ []     = []
dropWhile p (x:xs) | p x       = [] : dropWhile p xs
                   | otherwise = x : xs

--7.3a
map f = foldr (\x xs -> f x : xs) []

--7.3b
filter p = foldr (\x xs -> if p x then x : xs else xs ) []

--7.4
dec2int = foldl (\x y -> 10*x + y) 0

--7.5
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f(x,y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x,y) -> f x y

--7.6a
chop8 = unfold (== []) (take 8) (drop 8)

--7.6b
map f = unfold (== []) (\x -> f (head x)) (drop 1)

--7.6c
iterate f = unfold (const False) (id) (\x -> f x)

--7.7
import Data.Char
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
bin2int = foldr (\x y -> x + 2*y) 0
make8 bits = take 8 (bits ++ repeat 0)
parity bits = ((sum bits) `mod` 2)
addparity bits = parity bits : bits
encode = concat . map (addparity . make8 . int2bin . ord)
checkparity (bit:bits) = if bit == parity bits then bits else error "Parity check failed!"
paritychop []   = []
paritychop bits | length bits < 9 = error "Chop error, bits are missing"
                | otherwise       = checkparity (take 9 bits) : paritychop (drop 9 bits)
decode = map (chr . bin2int) . paritychop

--7.8
shittytransmit = decode . tail . encode
shittytransmit "roflcopter"

--7.9
altMap f1 f2 []     = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

--7.10
luhnDouble n | n >= 5    = (n * 2) - 9
             | otherwise = n * 2

luhn card = sum (altMap luhnDouble id card) `mod` 10 == 0