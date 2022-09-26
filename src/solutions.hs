{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List
import qualified System.Random as Rd

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

data ListItem a = Multiple Int a | Single a
  deriving (Show)

-- 11
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map trans $ encode xs
  where
    trans = \(c, x) -> case () of
      _
        | c == 1 -> Single x
        | otherwise -> Multiple c x

-- 12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified xs = foldl1 (++) $ map trans xs
  where
    trans = \case
      Single x -> [x]
      Multiple c x -> replicate c x

-- 13 same as 11

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) c = replicate c x ++ repli xs c

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs k = map fst $ filter notK $ tag xs
  where
    tag = \xs -> zip xs [1 ..]
    notK = \(_, id) -> mod id k /= 0

-- 17
split :: [a] -> Int -> ([a], [a])
split xs k = splitHelper [] xs k
  where
    splitHelper xs [] k = (xs, [])
    splitHelper xs ys 0 = (xs, ys)
    splitHelper xs (y : ys) k = splitHelper (xs ++ [y]) ys (k -1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs k1 k2 = fst $ split (snd $ split xs nk1) nk2
  where
    nk1 = k1 - 1
    nk2 = k2 - nk1

-- 19
rotate :: [a] -> Int -> [a]
rotate xs k = uncurry (flip (++)) $ split xs nk
  where
    nk = mod k $ length xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (xs !! nk, (init . fst) span ++ snd span)
  where
    nk = k -1
    span = split xs k

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = fst span ++ [x] ++ snd span
  where
    span = split xs (k -1)

-- 22
range :: Integral a => a -> a -> [a]
range x y
  | x == y = [x]
  | x > y = []
  | otherwise = x : range (x + 1) y

-- 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  gen <- Rd.getStdGen
  return $ take n [xs !! i | i <- Rd.randomRs (0, length xs - 1) gen]

-- 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- Rd.getStdGen
  return $ take n $ List.nub [i | i <- Rd.randomRs (1, m) gen]

-- 25
rndPermu :: [a] -> IO [a]
rndPermu xs = do
  perm <- diffSelect m m
  return [xs !! (i -1) | i <- perm]
  where
    m = length xs

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations k [] = []
combinations k (x : xs) = map (x :) (combinations (k -1) xs) ++ combinations k xs

-- 27
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (g : gs) xs = foldl1 (++) $ map (\x -> map (fst x :) (group gs (snd x))) $ comb g xs
  where
    comb 0 t = [([], t)]
    comb k [] = []
    comb k (x : xs) =
      map (first (x :)) (comb (k -1) xs)
        ++ map (second (x :)) (comb k xs)

-- 28
lsort :: Ord a => [[a]] -> [[a]]
lsort xs = sortBy length xs
  where
    sortBy _ [] = []
    sortBy f (x : xs) = sortBy f [y | y <- xs, f y <= f x] ++ (x : sortBy f [y | y <- xs, f y > f x])

-- 31
isPrime :: Integral a => a -> Bool
isPrime n = n >= 1 && all ((/= 0) . mod n) ps
  where
    ps = takeWhile (\x -> x * x <= n) (2 : [3, 5 ..])

-- 32
myGcd :: Integral a => a -> a -> a
myGcd a b
  | b == 0 = a
  | otherwise = myGcd b (mod a b)

-- 33
coprime :: Integral a => a -> a -> Bool
coprime a b = myGcd a b == 1

-- 34
totient :: Integral a => a -> a
totient n = fromIntegral $ length [() | x <- [1 .. n], coprime n x]

primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
      where
        (h, _ : t) = span (< p * p) xs
    sieve _ _ = error "impl error"

-- 35
primeFactors :: Integral a => a -> [a]
primeFactors n = go n primes
  where
    countp n p =
      if rem n p == 0
        then 1 + countp (div n p) p
        else 0
    go 1 _ = []
    go n (p : ps) = replicate c q ++ go nn ps
      where
        q = fromIntegral p
        c = countp n q
        nn = div n (q ^ c)
    go _ _ = error "impl error"
    go :: Integral a => a -> [Integer] -> [a]

-- 36
primeFactorsMult :: Integral a => a -> [(a, Int)]
primeFactorsMult n = go $ primeFactors n
  where
    go [] = []
    go xs@(x : _) = (x, length $ takeWhile (== x) xs) : go (dropWhile (== x) xs)

-- 37
eulerPhi :: Integral a => a -> a
eulerPhi n = foldl go n $ primeFactorsMult n
  where
    go n (p, _) = div n p * (p -1)

-- 39
primesR :: Integral a => a -> a -> [a]
primesR lower upper = takeWhile (<= upper) $ dropWhile (< lower) primes'
  where
    primes' = [fromIntegral p | p <- primes]

-- 40
goldbach :: Integral a => a -> (a, a)
goldbach n = to_pair $ head $ dropWhile go primes'
  where
    primes' = [fromIntegral p | p <- primes]
    to_pair x = (x, n - x)
    go x = not $ isPrime (n - x)

-- 41
goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList lower upper = map goldbach range
  where
    range = trans lower upper
    trans lower upper
      | even lower = [lower, lower + 2 .. upper]
      | otherwise = [lower + 1, lower + 3 .. upper]

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' a b = not' a `or'` b

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

-- 46
table :: (Bool -> Bool -> Bool) -> String
table f = foldl1 newline [to_str (x, y, f x y) | x <- domain, y <- domain]
  where
    domain = [True, False]
    to_str (x, y, z) = show x ++ " " ++ show y ++ " " ++ show z
    newline x y = x ++ "\n" ++ y

-- 47 same as 46
infixl 4 `or'`

infixl 4 `nor'`

infixl 5 `xor'`

infixl 6 `and'`

infixl 6 `nand'`

infixl 3 `equ'`

-- 48
tablen :: Int -> ([Bool] -> Bool) -> String
tablen n f = foldl1 (++) $ map to_str $ domains n
  where
    domains x
      | x <= 0 = []
      | x == 1 = [[True], [False]]
      | otherwise = go True rest ++ go False rest
      where
        go x = map (x :)
        rest = domains (x -1)
    to_str' [] = "\n"
    to_str' (x : t) = show x ++ " " ++ to_str' t
    to_str xs = to_str' $ xs ++ [f xs]

-- 49
gray :: Int -> [String]
gray n
  | n <= 0 = error "n should > 0"
  | n == 1 = ["0", "1"]
  | otherwise = s1 ++ s2
  where
    prev_gray = gray (n - 1)
    s1 = map ('0' :) prev_gray
    s2 = map ('1' :) $ reverse prev_gray

-- 50
huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman xs = go sorted_chars [] init_result
  where
    sorted_chars = map (\(c, n) -> ([c], n)) $ List.sortOn snd xs
    init_result = map (\x -> (fst x, "")) xs
    upd code c res = map (\(x, y) -> if c == x then (x, code : y) else (x, y)) res
    updList code cs res = foldl (\x f -> f x) res $ map (upd code) cs
    pick [] [] = (([], 0), [], [])
    pick (c : ct) [] = (c, ct, [])
    pick [] (h : ht) = (h, [], ht)
    pick cs@(c : ct) hs@(h : ht) =
      if snd c <= snd h
        then (c, ct, hs)
        else (h, cs, ht)
    go cs hs res
      | length cs == 1 && null hs = let ((q, _), _, _) = pick cs hs in updList '0' q res
      | length cs + length hs == 1 = res
      | otherwise =
        let ((q1, n1), cs1, hs1) = pick cs hs
            ((q2, n2), cs2, hs2) = pick cs1 hs1
            hs3 = hs2 ++ [(q1 ++ q2, n1 + n2)]
         in go cs2 hs3 $ updList '1' q2 $ updList '0' q1 res

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

-- 55
cbalTree :: Int -> [Tree Char]
cbalTree n
  | n == 0 = [Empty]
  | n == 1 = [Branch 'x' Empty Empty]
  | n1 == n2 = go s1 s2
  | otherwise = go s1 s2 ++ go s2 s1
  where
    n1 = (n - 1) `div` 2
    n2 = (n - 1) - n1
    s1 = cbalTree n1
    s2 = if n1 == n2 then s1 else cbalTree n2
    go s1 s2 = map (uncurry (Branch 'x')) $ (,) <$> s1 <*> s2
    go :: [Tree Char] -> [Tree Char] -> [Tree Char]

-- 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = symmetric' l r
  where
    symmetric' Empty Empty = True
    symmetric' Empty Branch {} = False
    symmetric' Branch {} Empty = False
    symmetric' (Branch _ l1 r1) (Branch _ l2 r2) =
      symmetric' l1 r2 && symmetric' r1 l2

-- 57
construct :: Ord a => [a] -> Tree a
construct = foldl ins Empty
  where
    ins Empty x = Branch x Empty Empty
    ins (Branch y l r) x
      | x <= y = Branch y (ins l x) r
      | otherwise = Branch y l (ins r x)

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
  | n == 0 = [Empty]
  | even n = []
  | otherwise = map (\x -> Branch 'x' x $ rev x) $ cbalTree nn
  where
    nn = n `div` 2
    rev Empty = Empty
    rev (Branch x l r) = Branch x (rev r) (rev l)

-- 59
hbalTree :: a -> Int -> [Tree a]
hbalTree c h
  | h < 0 = error "h must >= 0"
  | h == 0 = [Empty]
  | h == 1 = [Branch c Empty Empty]
  | otherwise = map (uncurry (Branch c)) all
  where
    t1 = hbalTree c (h -1)
    t2 = hbalTree c (h -2)
    all = ((,) <$> t1 <*> t2) ++ ((,) <$> t2 <*> t1) ++ ((,) <$> t1 <*> t1)