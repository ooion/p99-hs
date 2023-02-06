{-# LANGUAGE RankNTypes #-}

import Control.Monad (foldM, forM)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Bifunctor
import qualified Data.IntMap as Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Debug.Trace
import GHC.Arr (STArray (STArray), freezeSTArray)

-- 90
queens :: Int -> [[Int]]
queens n = dfs []
  where
    check xs x =
      notElem x xs
        && all (uncurry (/=)) (zip (map (abs . subtract x) xs) [1 ..])
    dfs xs
      | length xs == n = [xs]
      | otherwise =
        concatMap (\x -> dfs (x : xs)) $
          filter (check xs) [1 .. n]

-- 91
knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n started = dfs started [started]
  where
    next (a, b) paths = res
      where
        m = \x -> - x
        ds = [id, second m, bimap m m, first m] <*> [(1, 2), (2, 1)]
        res' = (\(a, b) (c, d) -> (a + c, b + d)) <$> ds <*> [(a, b)]
        res =
          filter
            ( \(a, b) ->
                a <= n && a >= 1 && b <= n && b >= 1
                  && notElem (a, b) paths
            )
            res'
    check_tour (a, b) = (`elem` [(1, 2), (2, 1)]) $ bimap (abs . (a -)) (abs . (b -)) started
    dfs s paths =
      let ss = next s paths
          rest_paths = concatMap (\s -> dfs s (paths ++ [s])) ss
       in if check_tour s then rest_paths ++ [paths] else rest_paths

closedKnights :: Int -> [[(Int, Int)]]
closedKnights n = go M.empty 1 (1, 1)
  where
    valid_coord (x, y) = x <= n && x >= 1 && y <= n && y >= 1
    valid visited xy = valid_coord xy && isNothing (M.lookup xy visited)
    moves = do
      p <- [(+), subtract]
      q <- [(+), subtract]
      let pp = p 1
      let qq = q 2
      [(pp, qq), (qq, pp)]

    get_solution visited = map fst $ sortOn snd $ M.toList visited
    next visited (x, y) = filter (valid visited) $ map (\(p, q) -> (p x, q y)) moves
    go visited m xy = res
      where
        visited' = M.insert xy m visited
        nexts = next visited xy
        nexts' = sortOn (length . next visited) nexts
        res =
          if m == n * n
            then [get_solution visited']
            else concatMap (go visited' (m + 1)) nexts'

-- 92
vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch edges = go M.empty M.empty 1
  where
    adde a (x, y) =
      zipWith
        ( \lst k ->
            if k == x
              then y : lst
              else if k == y then x : lst else lst
        )
        a
        [1 ..]
    adjs = listArray (1, n) $ foldl adde (replicate n []) edges
    en = length edges
    n = en + 1

    get_adj_val vx x = [v | y <- adjs ! x, (v, z) <- M.toList vx, z == y]
    vis_to_v vx = map fst $ sortOn snd $ M.toList vx
    go vx ve i
      | i == n + 1 = [vis_to_v vx]
      | otherwise = concatMap sub vals
      where
        avals = get_adj_val vx i
        get_diff val = map (\av -> abs (av - val)) avals
        valid_d d = d >= 1 && d <= en && isNothing (M.lookup d ve)
        valid val =
          isNothing (M.lookup val vx)
            && all (\av -> valid_d (abs (av - val))) avals
        upd val =
          let ve' = foldl (\v d -> M.insert d i v) ve (get_diff val)
           in (M.insert val i vx, ve')
        vals = filter valid [1 .. n]
        sub val = let (vx', ve') = upd val in go vx' ve' (i + 1)

-- 93
puzzle :: [Int] -> [String]
puzzle xs
  | n < 2 = []
  | otherwise = res
  where
    n = length xs
    res =
      concatMap
        ( \i ->
            let (l, r) = splitAt i xs
             in join (go l) (go r)
        )
        [1 .. (n -1)]
    join l r =
      [ ls ++ " = " ++ rs
        | (ls, lv) <- l,
          (rs, rv) <- r,
          lv == rv
      ]

    gen_ops 0 = [""]
    gen_ops n = [x : xs | x <- "+-/*", xs <- gen_ops (n -1)]
    go xs = res
      where
        n = length xs
        opss = gen_ops (n -1)
        parens = genFineParen (n + 1)
        candidates = [(ops, paren) | ops <- opss, paren <- parens, valid ops paren]
        xs' :: [Ratio Int]
        xs' = map fromIntegral xs
        res = do
          (ops, paren) <- candidates
          let orders = calcOrders paren ops
          let (ans, div_zero) = calc orders ops xs'
          [(calcS paren ops xs, ans) | not div_zero]

    calcS paren ops xs = concat sxs'''
      where
        sxs = map show xs
        sxs' =
          zipWith
            ( \p x ->
                if p <= 0
                  then x
                  else replicate p '(' ++ x
            )
            paren
            sxs
        sxs'' =
          zipWith
            ( \p x ->
                if p >= 0
                  then x
                  else x ++ replicate (- p) ')'
            )
            (tail paren)
            sxs'
        sxs''' = head sxs'' : zipWith (:) ops (tail sxs'')

    calc orders ops xs = aux orders ops (tail xs) [head xs] [] False
      where
        upd (xs, True) _ = (xs, True)
        upd (x : y : xs, _) op
          | op == '+' = ((y + x) : xs, False)
          | op == '-' = ((y - x) : xs, False)
          | op == '*' = ((y * x) : xs, False)
          | op == '/' = if x /= 0 then ((y / x) : xs, False) else (y : xs, True)
          | otherwise = error "invalid op"
        upd _ _ = error "invalid pattern 1"

        aux [] _ _ xbuf oopbuf div_zero =
          let (xbuf', div_zero') =
                foldl (\xb oop -> upd xb (snd oop)) (xbuf, div_zero) oopbuf
           in (head xbuf', div_zero')
        aux (o : os) (op : ops) (x : xs) xbuf oopbuf div_zero =
          let obuf0 = takeWhile ((>= o) . fst) oopbuf
              obuf1 = dropWhile ((>= o) . fst) oopbuf
              (xbuf', div_zero') =
                foldl (\xb oop -> upd xb (snd oop)) (xbuf, div_zero) obuf0
           in aux os ops xs (x : xbuf') ((o, op) : obuf1) div_zero'
        aux _ _ _ _ _ _ = error "invalid pattern 2"

    valid ops parr = orders `notElem` left_orders
      where
        orders = calcOrders parr ops
        n = length parr
        ps = toParenPairs parr
        left_order p = calcOrders (toParenArr n $ filter (/= p) ps) ops
        left_orders = map left_order ps

    calcOrders parr ops = normalize orders
      where
        paren_pri = 100
        normalize os =
          let sorted_os = nub $ sort os
           in map (fromJust . (`elemIndex` sorted_os)) os

        pri op = if op `elem` "+-" then 0 else div paren_pri 2
        bases =
          init . drop 2 . map fst $
            scanl
              ( \(b, las) d ->
                  if d < 0
                    then (b + paren_pri * d + las, 0)
                    else (b + las, paren_pri * d)
              )
              (0, 0)
              parr
        orders =
          zipWith3
            (\b op i -> i + b + pri op)
            bases
            ops
            (reverse [1 .. length ops])

    genFineParen n = filter valid parens
      where
        parens = genGrossParen n
        valid p = (0, n -1) `notElem` ps && length ps == length ps'
          where
            ps = toParenPairs p
            ps' = nub ps

    toParenArr n pairs = res
      where
        aux i (x, y)
          | i == x = 1
          | i == y = -1
          | otherwise = 0
        res = [sum (map (aux i) pairs) | i <- [0 .. (n -1)]]

    toParenPairs parr = c
      where
        expand d =
          if d >= 0
            then replicate d 1
            else replicate (- d) (-1)
        a =
          concatMap (\(x, i) -> zip (expand x) (repeat i)) $
            zip parr [0 :: Int ..]
        find vi k =
          (snd . head) $
            dropWhile ((> 0) . fst) $
              scanl1 (\(x, _) (y, j) -> (x + y, j)) $ drop k a
        b = filter ((> 0) . fst . fst) $ zip a [0 ..]
        c = map (\((v, i), k) -> (i, find (v, i) k)) b

    genGrossParen n = gross 0 [] 0
      where
        gross x a d
          | x == n = [reverse a | d == 0]
          | otherwise =
            let m = n - d - x - 1
                res = concatMap (\i -> gross (x + 1) (i : a) (d + i)) [(- d) .. m]
             in res

-- 94
regular :: Int -> Int -> [[(Int, Int)]]
regular n d
  | odd n && odd d = []
  | d == 0 = [[]]
  | n == 1 = []
  | otherwise = graphs''
  where
    graphs = runST (go 1 2 state [])
    graphs' = nub $ map sort graphs
    graphs'' = nub $ map ming graphs'
    perms = perm n
    trans p g = map (transe . bimap (p !) (p !)) g
    transe (x, y) = if x < y then (x, y) else (y, x)
    ming g = minimum $ map (sort . (`trans` g)) perms

    perm m = runST $ perm' m sarr
      where
        sarr = newListArray (1, m) [1 .. m] :: ST s (STArray s Int Int)
        perm' n sa
          | n == 1 = do a <- sa; lst <- freezeSTArray a; pure [lst]
          | otherwise = do
            a <- sa
            init <- perm' (n -1) (pure a)
            res <- forM [1 .. (n -1)] $ \i -> do
              iv <- readArray a i
              nv <- readArray a n
              writeArray a i nv
              writeArray a n iv
              res <- perm' (n -1) (pure a)
              writeArray a i iv
              writeArray a n nv
              pure res
            pure $ concat (init : res)

    state = newArray (1, n) d :: ST s (STArray s Int Int)
    go :: Int -> Int -> ST s (STArray s Int Int) -> [(Int, Int)] -> ST s [[(Int, Int)]]
    go x y ds es
      | x == n = do arr <- ds; d <- readArray arr n; pure [es | d == 0]
      | y == n + 1 = do arr <- ds; d <- readArray arr x; if d == 0 then go (x + 1) (x + 2) ds es else pure []
      | otherwise = do
        arr <- ds
        dx <- readArray arr x
        dy <- readArray arr y
        let d = minimum [dx, dy, 1]
        subs <- forM [0 .. d] $ \di -> do
          writeArray arr x (dx - di)
          writeArray arr y (dy - di)
          let es' = if di == 0 then es else (x, y) : es
          res <- go x (y + 1) (pure arr) es'
          writeArray arr x dx
          writeArray arr y dy
          pure res
        pure $ concat subs

-- 95
fullWords :: Int -> String
fullWords num =
  let num2word =
        listArray
          (0, 9)
          [ "zero",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine"
          ]
   in intercalate "-" $
        map ((num2word !) . snd) $
          reverse $
            takeWhile ((> 0) . fst) $
              iterate
                ( \(n, _) ->
                    let n' = div n 10 in (n', rem n' 10)
                )
                (num, rem num 10)

-- 96
identifier :: String -> Bool
identifier s = check s True
  where
    check [] b = b
    check ('-':xs) b = b && check xs False
    check (_:xs) _ = check xs True
    
