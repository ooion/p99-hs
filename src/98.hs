import Data.List

genRowCandidates :: Int -> [Int] -> [[Int]]
genRowCandidates n rowXs = go 0 rowXs []
  where
    apto m t xs = xs ++ replicate (m - length xs) t
    go _ [] xs = [apto n 0 xs]
    go s (r : rs) xs =
      let fix a xs = (apto (a + r) 1 . apto a 0) xs
       in concatMap (\a -> go (a + r + 1) rs (fix a xs)) [s .. (n - r)]

solve :: [[Int]] -> [[Int]] -> [[Int]]
solve rowXs colXs = reverse . snd $ go candidates colXs zrow []
  where
    ncol = length colXs
    zrow = replicate ncol (0 :: Int)
    candidates = map (genRowCandidates ncol) rowXs
    sel xs pre_xs cols = (ok, cols')
      where
        z3 = zip3 pre_xs xs cols
        f (px, x, c)
          | x > 0 = case c of
            [] -> (False, c)
            (c' : cs') -> (c' > 0, (c' -1) : cs')
          | px > 0 = case c of
            [] -> (False, c)
            (c' : cs') -> (c' == 0, cs')
          | otherwise = (True, c)
        ok_cols = map f z3
        cols' = map snd ok_cols
        ok = all fst ok_cols
    go [] cols pre_xs rows =
      let (ok, _) = sel zrow pre_xs cols
       in (ok, rows)
    go (cand : cands) cols pre_xs rows = res
      where
        res = foldl (\(ok, rs) xs -> if ok then (ok, rs) else sel' xs) (False, []) cand
        sel' xs = case sel xs pre_xs cols of
          (ok, cols') -> if ok then go cands cols' xs (xs : rows) else (False, rows)

nonogram :: [[Int]] -> [[Int]] -> String
nonogram rowXs colXs = unlines (p1_lines ++ p2_lines)
  where
    xs = solve rowXs colXs
    rowXs2s rxs = concatMap (\r -> " " ++ show r) rxs
    -- part 1 lines
    p1_lines =
      let zs = zip xs rowXs
          x2g x = if x == 0 then "_|" else "X|"
          line2s s = '|' : concatMap x2g s
          f (s, rxs) = line2s s ++ rowXs2s rxs
       in map f zs
    -- part 2 lines
    p2_lines =
      let cols' = transpose colXs
       in map rowXs2s cols'

main :: IO ()
main = putStr $ nonogram [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]] [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]]