{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import Data.List
import qualified Data.Ord

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

newtype Adjacency a = Adj [(a, [a])]
  deriving (Show, Eq)

-- 80
graphToAdj :: Eq a => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (v : vs) es) =
  let Adj as = graphToAdj (Graph vs es)
      f (a, b)
        | a == v = [b]
        | b == v = [a]
        | otherwise = []
   in Adj ((v, es >>= f) : as)

adjToGraph :: Eq a => Adjacency a -> Graph a
adjToGraph (Adj []) = Graph [] []
adjToGraph (Adj ((x, xs) : as)) =
  let Graph vs es = adjToGraph (Adj as)
      visited (a, b) = ((a, b) `elem` es) || ((b, a) `elem` es)
      es' = filter (not . visited) $ map (x,) xs
   in Graph (x : vs) (es' ++ es)

-- 81
paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths a b es = if a == b then [[b]] else res
  where
    (es1, es2) = partition ((== a) . fst) es
    vs = map snd es1
    res = concatMap (\c -> map (a :) $ paths c b es2) vs

-- 82
cycle :: Eq a => a -> [(a, a)] -> [[a]]
cycle a es = res
  where
    vs = map snd $ filter ((== a) . fst) es
    res = concatMap (\c -> map (a :) $ paths c a es) vs

-- 83
k4 =
  Graph
    ['a', 'b', 'c', 'd']
    [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

spanningTree :: Eq a => Graph a -> [Graph a]
spanningTree (Graph [] _) = []
spanningTree g@(Graph [x] _) = [g]
spanningTree (Graph xs es) =
  map mkGraph $
    filter ((== n) . length . getVsFromEs) $
      filter ((== n -1) . length) all
  where
    n = length xs
    xine x (a, b) = x == a || x == b
    getVsFromEs [] = []
    getVsFromEs ((a, b) : es) = [p | p <- [a, b], not (any (xine p) es)] ++ getVsFromEs es
    mkGraph es = Graph (getVsFromEs es) es
    getall [] = [[]]
    getall (e : es) = map (e :) ess ++ ess
      where
        ess = getall es
    all = getall es

-- 84
prim :: (Eq a, Ord b) => [a] -> [(a, a, b)] -> [(a, a, b)]
prim [] _ = []
prim (x : xs) es = go xs es2 ds
  where
    xine x (a, b, _) = a == x || b == x
    (es1, es2) = partition (xine x) es
    other t (a, b, _) = if a == t then b else a
    es_to_ds e@(a, b, _) = (other x e, e)
    ds = map es_to_ds es1
    dsel (x1, e1@(_, _, d1)) (x2, e2@(_, _, d2))
      | d1 < d2 = (x1, e1)
      | otherwise = (x2, e2)
    go [] _ _ = []
    go _ _ [] = []
    go xs es ds = e : go xs' es' ds'
      where
        (x, e) = foldl1 dsel ds
        xs' = filter (/= x) xs
        ds1 = filter ((/= x) . fst) ds
        (es1, es') = partition (xine x) es
        ds2 = map (\e -> (other x e, e)) $ filter (flip elem xs' . other x) es1
        ds3 = [dsel d1 d2 | d1 <- ds1, d2 <- ds2, fst d1 == fst d2]
        ds3x = map fst ds3
        ds1' = filter (not . (`elem` ds3x) . fst) ds1
        ds2' = filter (not . (`elem` ds3x) . fst) ds2
        ds' = ds1' ++ ds2' ++ ds3

-- 85
iso :: Eq a => Graph a -> Graph a -> Bool
iso ag bg@(Graph bxs bes) = ag_t `elem` bgs_t
  where
    locate [] _ = 0
    locate (x : xs) y = if x == y then 1 else 1 + locate xs y
    trans (Graph xs es) = Graph [1 .. n] es''
      where
        n = length xs
        es' = map (bimap (locate xs) (locate xs)) es
        es'' = sort es'
    perm [] = [[]]
    perm (x : xs) =
      map (\(a, b) -> a ++ (x : b)) $
        map splitAt [0 .. n] <*> ps
      where
        n = length xs
        ps = perm xs
    ag_t = trans ag
    bgs_t = map (trans . (`Graph` bes)) $ perm bxs

--86
kColor :: Eq a => [a] -> [(a, a)] -> [(a, Int)]
kColor xs es = go dxs es 1
  where
    xine x (a, b) = x == a || x == b
    xines x es = any (xine x) es
    eines (a, b) es = ((a, b) `elem` es) || ((b, a) `elem` es)
    deg x = foldl (\a b -> if xine x b then a + 1 else a) 0 es
    degs = map (\x -> (deg x, x)) xs
    dxs = map snd $ sortOn (Data.Ord.Down . fst) degs

    sel_color = sel_color' []
    sel_color' ss [] = ss
    sel_color' ss (x : xs) =
      let ess = map (x,) ss
          ss' = if any (`eines` es) ess then ss else x : ss
       in sel_color' ss' xs
    go [] _ k = []
    go xs es k = ks ++ go xs' es (k + 1)
      where
        s_xs = sel_color xs
        xs' = filter (not . (`elem` s_xs)) xs
        ks = map (,k) s_xs

-- 87
depthFirst :: Eq a => ([a], [(a, a)]) -> a -> [a]
depthFirst (xs, es) x = dfs x []
  where
    xine x (a, b) = x == a || x == b
    other t (a, b) = if a == t then b else a
    get_adjs x = map (other x) $ filter (xine x) es
    dfs x vs
      | x `elem` vs = vs
      | otherwise = foldl (flip dfs) (x : vs) $ get_adjs x

-- 88
connectedComponents :: Eq a => ([a], [(a, a)]) -> [[a]]
connectedComponents (xs, es) = go xs
  where
    go [] = []
    go xs@(x : t) =
      let c = depthFirst (xs, es) x
          xs' = filter (not . (`elem` c)) xs
       in c : go xs'

-- 89
bipartite :: Eq a => ([a], [(a, a)]) -> Bool
bipartite ([], _) = True
bipartite (x : xs, es) = aux [x] [] es [x] []
  where
    xine x (a, b) = x == a || x == b
    einxs (a, b) xs = (a `elem` xs) && (b `elem` xs)
    other t (a, b) = if a == t then b else a
    get_adjs es x = map (other x) $ filter (xine x) es
    go (xs, es) =
      ( nub (concatMap (get_adjs es) xs),
        filter (not . (\e -> any (`xine` e) xs)) es
      )
    aux [] _ es vxs vys =
      let nxs = (xs \\ vxs) \\ vys in bipartite (nxs, es)
    aux xs ys es vxs vys =
      let (ys', es') = go (xs, es)
          (xs', es'') = go (ys', es')
          self_sect = any (`einxs` xs') es || any (`einxs` ys') es
          exist_sect = any (`elem` vxs) ys' || any (`elem` vys) xs'
       in not (self_sect || exist_sect)
            && aux xs' ys' es'' (xs' ++ vxs) (ys' ++ vys)
