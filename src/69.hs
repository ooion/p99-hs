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
