
-- 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

encode :: (Num a, Eq b) => [b] -> [(a, b)]
encode [] = []
encode (x:xs) = encode' x 1 xs
  where
    encode' prev i [] = (i,prev):[]
    encode' prev i (x:xs)
      | prev == x = encode' x (i+1) xs
      | otherwise = (i,prev):(encode' x 1 xs)

-- 12
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeItem x) ++ (decodeModified xs)
  where
    decodeItem (Single a) = a:[]
    decodeItem (Multiple n a) | n == 1    = a:[]
                              | otherwise = a:(decodeItem (Multiple (n-1) a))

-- 13
-- See problem 11

-- 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:(dupli xs)

-- 15
repli :: [a] -> Integer -> [a]
repli [] n     = []
repli (x:xs) n = (repli' n x) ++ (repli xs n)
  where
    repli' 1 x = [x]
    repli' n x = x:(repli' (n-1) x)

-- 16
dropEvery :: [a] -> Integer -> [a]
dropEvery [] n   = []
dropEvery list n = dropEvery' list n 1
  where
    dropEvery' [] n i = []
    dropEvery' (x:xs) n i | n == i    = dropEvery' xs n 1
                          | otherwise = x:dropEvery' xs n (i+1)

-- 17
split :: [a] -> Integer -> [[a]]
split [] n     = []
split l n = split' [] l n 0
  where
    split' f [] n i = []
    split' f (x:xs) n i | n == i = f:((x:xs):[])
                        | otherwise = split' (f ++ [x]) xs n (i+1)

-- 18
slice :: [a] -> Integer -> Integer -> [a]
slice [] s e = []
slice list s e =  head (split (last (split list (s-1))) (e-s+1))

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n = drop nn xs ++ take nn xs
    where
      nn = n `mod` length xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n [] = error "Array index out of bounds"
removeAt n xs = (elementAt n xs, take (n-1) xs ++ drop (n) xs)

-- From problem 3
elementAt :: Int -> [a] -> a
elementAt n l = elementAt' n 1 l
  where
    elementAt' _ _ [] = error "Array index out of bounds"
    elementAt' n i (x:xs)
      | n == i    = x
      | otherwise = elementAt' n (i+1) xs
