import Data.List

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

newtype Adjacency a = Adj [(a, [a])]
  deriving (Show, Eq)

-- 80
graphToAdj :: Eq a => Graph a -> Adjacency a
graphToAdj (Graph vs es) = Adj (map (aux es) vs)
  where
    ine v (v1, v2) = v == v1 || v == v2
    otherv v (v1, v2) = if v == v1 then v2 else v1
    go x es = map (otherv x) $ filter (ine x) es
    aux es x = (x, go x es)

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

-- 84
prim :: (Eq a, Ord b) => [a] -> [(a, a, b)] -> [(a, a, b)]
prim [] es = []
prim (v : vs) es = if null es1 then [] else e : prim vs es2
  where
    ine (v1, v2, _) = v == v1 || v == v2
    (es1, es2) = partition ine es
    sel e1@(_, _, x) e2@(_, _, y) = if x < y then e1 else e2
    e = foldl1 sel es1
