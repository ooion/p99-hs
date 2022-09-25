-- 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x : xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast = last . init

-- 3
elementAt :: [a] -> Int -> a
elementAt (x : xs) 1 = x
elementAt [] _ = error ""
elementAt (x : xs) t = elementAt xs (t -1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = myLength xs + 1

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

data NestedList a = Elem a | List [NestedList a]

-- 7
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (\x y -> x ++ flatten y) [] xs

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : y : z)
  | x == y = compress (y : z)
  | otherwise = x : compress (y : z)

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : y : z)
  | x == y = (x : head ps) : tail ps
  | otherwise = [x] : ps
  where
    ps = pack (y : z)

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x : y : z)
  | x == y = inc (head es) : tail es
  | otherwise = (1, x) : es
  where
    es = encode (y : z)
    inc = \(c, x) -> (c + 1, x)