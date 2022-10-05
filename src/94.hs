import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Data.Maybe

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
