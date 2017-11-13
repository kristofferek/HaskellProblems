-- 1
lastElem :: [a] -> a
lastElem [] = error "Empty list"
lastElem (x:[]) = x
lastElem (_:xs) = lastElem xs

-- 2
secondLastElem :: [a] -> a
secondLastElem [] = error "Empty list"
secondLastElem (x:[]) = x -- Returns first elem if list has only one elem
secondLastElem (x:(_:[])) = x
secondLastElem (_:xs) = secondLastElem xs

-- 3
elementAt :: Integer -> [a] -> a
elementAt n l = elementAt' n 1 l
  where
    elementAt' _ _ [] = error "Array index out of bounds"
    elementAt' n i (x:xs)
      | n == i    = x
      | otherwise = elementAt' n (i+1) xs

-- 4
sizeOf :: [a] -> Integer
sizeOf [] = 0
sizeOf (x:xs) = 1 + sizeOf xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = myReverse' [] list
  where
    myReverse' a (x:[]) = (x:a)
    myReverse' a (x:xs) = myReverse' (x:a) xs

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = listEqual list (myReverse list)

listEqual :: Eq a => [a] -> [a] -> Bool
listEqual [] [] = True
listEqual (x:xs) (y:ys) | x == y = listEqual xs ys
listEqual _ _ = False

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x:(compress' x xs)
  where
    compress' prev [] = []
    compress' prev (x:xs)
      | prev == x = compress' x xs
      | otherwise = x:(compress' x xs)

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' x [x] xs
  where
    pack' _ sub [] = sub:[]
    pack' prev sub (x:xs)
      | prev == x = pack' x (x:sub) xs
      | otherwise = sub:(pack' x [x] xs)

-- 10
encode :: (Num a, Eq b) => [b] -> [(a, b)]
encode [] = []
encode (x:xs) = encode' x 1 xs
  where
    encode' prev i [] = (i,prev):[]
    encode' prev i (x:xs)
      | prev == x = encode' x (i+1) xs
      | otherwise = (i,prev):(encode' x 1 xs)
