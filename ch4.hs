--4.1
halve ns = (take (length ns `div` 2) ns, drop (length ns `div` 2) ns)

--4.2a
third ns = head tail tail ns
--4.2b
third ns = ns !! 2
--4.2c
third (_:_:a:_) = a

--4.3a
safetail ns = if null ns then [] else tail ns
--4.3b
safetail ns | null ns   = []
            | otherwise = tail ns
--4.3c
safetail [] = []
safetail ns = tail ns

--4.4
True || True   = True
True || False  = True
False || True  = True
False || False = False

False || False = False
_ || _         = True

False || b = b
True || _  = True

b || c | b == c    = b
       | otherwise = True

--4.5
newand a b = if a == True then if b == True then True else False else False

--4.6
newnewand a b = if a == True then b else False

--4.7
newmult = \x -> (\y -> x * (\z -> y * z))

--4.8
luhnDouble n | n >= 5    = (n * 2) - 9
             | otherwise = n * 2

luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10