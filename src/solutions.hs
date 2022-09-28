{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad (forM, forM_)
-- import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List
import GHC (RecordConTc (rcon_con_expr))
import GHC.RTS.Flags (GCFlags (maxHeapSize))
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
    go :: [Tree Char] -> [Tree Char] -> [Tree Char]
    go s1 s2 = map (uncurry (Branch 'x')) $ (,) <$> s1 <*> s2

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

-- 60
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x 0 = [Empty]
hbalTreeNodes x 1 = [Branch x Empty Empty]
hbalTreeNodes x n = concatMap (aux n) hs
  where
    maxn h = 2 ^ h - 1
    minn h = ns !! h
      where
        ns = 0 : scanl (\x y -> x + y + 1) 1 ns
    spawn n h = filter match $ concatMap (pairs n (h -1)) [0 .. n]
      where
        pairs 0 _ _ = []
        pairs n h x =
          let y = n - 1 - x
           in [ ((x, h), (y, h)),
                ((x, h), (y, h -1)),
                ((x, h -1), (y, h))
              ]
        match (a, b) =
          let f (n, h) = h >= 0 && n >= 0 && n >= minn h && n <= maxn h
           in f a && f b
    hs = dropWhile ((< n) . maxn) $ takeWhile ((<= n) . minn) [1 ..]
    aux 0 0 = [Empty]
    aux n h = map (uncurry (Branch x)) all
      where
        pairs = spawn n h
        aux' = uncurry aux
        all = concatMap (\(a, b) -> (,) <$> aux' a <*> aux' b) pairs

tree4 =
  Branch
    1
    (Branch 2 Empty (Branch 4 Empty Empty))
    (Branch 2 Empty Empty)

-- 61
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- 61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = internals l ++ (x : internals r)

-- 62B
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x l r) n = atLevel l (n -1) ++ atLevel r (n -1)

-- 63
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch 'x' (completeBinaryTree leftn) (completeBinaryTree rightn)
  where
    pow2 = 1 : map (* 2) pow2
    lastn = last $ takeWhile (>= 0) $ scanl (-) n pow2
    subn = div (n -1 - lastn) 2
    maxlastn = subn + 1
    leftlastn = min (subn + 1) lastn
    rightlastn = lastn - leftlastn
    leftn = subn + leftlastn
    rightn = subn + rightlastn

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = count t == getMax t 1
  where
    count Empty = 0
    count (Branch _ l r) = 1 + count l + count r
    getMax Empty i = 0
    getMax (Branch _ l r) i = max i $ max (getMax l 2 * i) $ getMax r (2 * i + 1)

tree64 =
  Branch
    'n'
    ( Branch
        'k'
        ( Branch
            'c'
            (Branch 'a' Empty Empty)
            ( Branch
                'h'
                ( Branch
                    'g'
                    (Branch 'e' Empty Empty)
                    Empty
                )
                Empty
            )
        )
        (Branch 'm' Empty Empty)
    )
    ( Branch
        'u'
        ( Branch
            'p'
            Empty
            ( Branch
                's'
                (Branch 'q' Empty Empty)
                Empty
            )
        )
        Empty
    )

-- 64
layout :: Tree a -> Tree (a, (Int, Int))
layout t = fst $ go t 0 1
  where
    go Empty _ _ = (Empty, 0)
    go (Branch q l r) x y =
      let (tl, cl) = go l x (y + 1)
          (tr, cr) = go r (1 + cl) (y + 1)
       in (Branch (q, (cl + 1, y)) tl tr, cl + cr + 1)

tree65 =
  Branch
    'n'
    ( Branch
        'k'
        ( Branch
            'c'
            (Branch 'a' Empty Empty)
            ( Branch
                'e'
                (Branch 'd' Empty Empty)
                (Branch 'g' Empty Empty)
            )
        )
        (Branch 'm' Empty Empty)
    )
    ( Branch
        'u'
        ( Branch
            'p'
            Empty
            (Branch 'q' Empty Empty)
        )
        Empty
    )

-- 65
layout' :: Tree a -> Tree (a, (Int, Int))
layout' Empty = Empty
layout' t = go t (rootx t rootSep) 1 halfRootSep
  where
    depth Empty = 0
    depth (Branch _ l r) = 1 + max (depth l) (depth r)
    maxy = depth t
    rootSep = 2 ^ (maxy - 1)
    halfRootSep = div rootSep 2
    rootx Empty s = 1 - s
    rootx (Branch _ l _) s = sh + rootx l sh
      where
        sh = div s 2
    go Empty _ _ _ = Empty
    go (Branch q l r) x y s =
      let tl = go l (x - s) (y + 1) (div s 2)
          tr = go r (x + s) (y + 1) (div s 2)
       in Branch (q, (x, y)) tl tr

-- 66
layout'' :: Tree a -> Tree (a, (Int, Int))
layout'' t = updx nt xbias
  where
    aux Empty _ = ([], [], Empty)
    aux (Branch a l r) y =
      let (lx1, lx2, lt) = aux l (y + 1)
          (rx1, rx2, rt) = aux r (y + 1)
          seps = zipWith (\a b -> div (a - b + 2) 2) lx2 rx1
          sep = if null seps then 1 else maximum seps
          x1 = 0 : map (subtract sep) lx1
          x2 = 0 : map (+ sep) rx2
       in (x1, x2, Branch (a, (0, y)) (updx lt (- sep)) (updx rt sep))
    updx Empty _ = Empty
    updx (Branch (a, (x, y)) l r) ax =
      Branch (a, (x + ax, y)) (updx l ax) (updx r ax)
    (x1, _, nt) = aux t 1
    xbias = if null x1 then 1 else 1 - minimum x1

-- 67A
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c l r) = c : '(' : content ++ ")"
  where
    content = treeToString l ++ "," ++ treeToString r

stringToTree :: (MonadFail m) => String -> m (Tree Char)
stringToTree "" = return Empty
stringToTree [c] = return $ Branch c Empty Empty
stringToTree str = tfs str >>= \("", t) -> return t
  where
    tfs a@(x : xs)
      | x == ',' || x == ')' = return (a, Empty)
    tfs (x : y : xs)
      | y == ',' || y == ')' = return (y : xs, Branch x Empty Empty)
      | y == '(' = do
        (',' : xs', l) <- tfs xs
        (')' : xs'', r) <- tfs xs'
        return (xs'', Branch x l r)
    tfs _ = fail "bad parse"

-- 68
treeToPreorder :: Tree Char -> String
treeToPreorder Empty = []
treeToPreorder (Branch c l r) = c : treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = []
treeToInorder (Branch c l r) = treeToInorder l ++ c : treeToInorder r

preInTree :: MonadFail m => String -> String -> m (Tree Char)
preInTree [] [] = return Empty
preInTree po@(x : xs) io = do
  (lio, _ : rio) <- return $ break (== x) io
  (lpo, rpo) <- return $ splitAt (length lio) xs
  l <- preInTree lpo lio
  r <- preInTree rpo rio
  return $ Branch x l r
preInTree _ _ = fail "woops"

preInTree' :: Monad m => String -> String -> m (Tree Char)
preInTree' [] [] = return Empty
preInTree' po io = pure act_t
  where
    (act_t, _) = foldl upd (Empty, []) (zip po io)
    upd (t, xs) (p, i) = (ins p xs t, i : xs)
    ins x _ Empty = Branch x Empty Empty
    ins x xs (Branch y l r)
      | x `elem` xs = Branch y l (ins x xs r)
      | otherwise = Branch y (ins x xs l) r

-- 69
ds2tree :: String -> (Tree Char, String)
ds2tree [] = (Empty, "")
ds2tree ('.' : xs) = (Empty, xs)
ds2tree (x : xs) = (Branch x l r, xs'')
  where
    (l, xs') = ds2tree xs
    (r, xs'') = ds2tree xs'

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch c l r) = (c : tree2ds l) ++ tree2ds r

data MTree a = Node a [MTree a]
  deriving (Eq, Show)

mtree1 = Node 'a' []

mtree2 = Node 'a' [Node 'b' []]

mtree3 = Node 'a' [Node 'b' [Node 'c' []]]

mtree4 = Node 'b' [Node 'd' [], Node 'e' []]

mtree5 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

-- 70C
nnodes :: MTree a -> Int
nnodes (Node _ children) = foldl upd 1 children
  where
    upd n t = n + nnodes t

-- 70
mtreeToString :: MTree Char -> String
mtreeToString (Node x []) = x : "^"
mtreeToString (Node x cs) = (x : concatMap mtreeToString cs) ++ "^"

stringToMtree :: String -> MTree Char
stringToMtree xs = fst $ go xs
  where
    go (x : '^' : xs) = (Node x [], xs)
    go (x : xs) = let (cs, xs') = goc xs in (Node x cs, xs')
    go [] = error "parse error"
    goc ('^' : xs) = ([], xs)
    goc xs =
      let (c, xs') = go xs
          (cs, xs'') = goc xs'
       in (c : cs, xs'')

-- 71
ipl :: MTree a -> Int
ipl = ipl' 1
  where
    ipl' h (Node _ cs) =
      h * length cs + sum (map (ipl' (h + 1)) cs)

-- 72
bottomUp :: MTree Char -> String
bottomUp (Node x []) = [x]
bottomUp (Node x cs) = concatMap bottomUp cs ++ [x]

-- 73
lisp :: MTree Char -> String
lisp (Node x []) = [x]
lisp (Node x cs) = ('(' : [x]) ++ content' ++ ")"
  where
    content = unwords $ map lisp cs
    content' = if null content then content else ' ' : content