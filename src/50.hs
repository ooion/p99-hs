import Data.List

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
    sorted_chars = map (\(c, n) -> ([c], n)) $ sortOn snd xs
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