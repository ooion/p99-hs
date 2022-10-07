{-# LANGUAGE RankNTypes #-}

import Cmm (CmmNode (res))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Bifunctor
import qualified Data.IntMap as Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import GHC.Arr (STArray (STArray), freezeSTArray)
import GHCi.Message (QState (qsLocation))

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
    ming g = minimum $ map (sort.(`trans` g)) perms

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
