import Data.Array (Array, listArray, (!))
import Data.Bifunctor (second)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (group, groupBy, maximum, minimumBy, nub, sort, sortOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import qualified Maybes as Data.Maybe
import Util (uncurry3)

data Puzzle = Puzzle [String] [String]

-- r c len
data PSeg = Vert Int Int Int | Hori Int Int Int

data CanSet = CanSet Int IntSet

genRowSeg :: [String] -> [(Int, Int, Int)]
genRowSeg pts = segs
  where
    f (c, ss) s
      | head s == ' ' || lens == 1 = (c + lens, ss)
      | otherwise = (c + lens, (c, lens) : ss)
      where
        lens = length s
    go xs = snd $ foldl f (1, []) (group xs)
    addr r (c, l) = (r, c, l)
    segs = concat $ zipWith (\r xs -> map (addr r) (go xs)) [1 ..] pts

genSegs :: [String] -> [PSeg]
genSegs pts = segs
  where
    h_segs = genRowSeg pts
    v_segs = genRowSeg (transpose pts)
    segs = map (uncurry3 Hori) h_segs ++ map (uncurry3 (flip Vert)) v_segs

check :: Puzzle -> ([Int], [Int])
check (Puzzle words pts) = (ws, ps)
  where
    ws = sort $ map length words
    segs = genSegs pts
    gg (Vert _ _ l) = l
    gg (Hori _ _ l) = l
    ps = sort $ map gg segs

readCrossword :: String -> Puzzle
readCrossword s = Puzzle words (fix pts)
  where
    ls = lines s
    words = takeWhile (not . null) ls
    pts = filter (not . null) $ dropWhile (not . null) ls

    -- for .dat testfile that lack space at last line
    fix pts =
      let n = length (head pts)
          las = last pts
          las' = las ++ replicate (n - length las) ' '
       in init pts ++ [las']

earlyCheck :: [String] -> [PSeg] -> Bool
earlyCheck words segs = ps == ws
  where
    ws = sort $ map length words
    lseg (Vert _ _ l) = l
    lseg (Hori _ _ l) = l
    ps = sort $ map lseg segs

hasIntersected :: PSeg -> PSeg -> Bool
hasIntersected Vert {} Vert {} = False
hasIntersected Hori {} Hori {} = False
hasIntersected (Vert r1 c1 l1) (Hori r2 c2 l2) =
  r1 <= r2 && r1 + l1 > r2 && c2 <= c1 && c2 + l2 > c1
hasIntersected h v = hasIntersected v h

wordPos :: PSeg -> PSeg -> (Int, Int)
wordPos Vert {} Vert {} = error "no intersect"
wordPos Hori {} Hori {} = error "no intersect"
wordPos (Vert r1 c1 _) (Hori r2 c2 _) = (r2 - r1, c1 - c2)
wordPos (Hori r1 c1 _) (Vert r2 c2 _) = (c2 - c1, r1 - r2)

segLength :: PSeg -> Int
segLength (Vert _ _ l) = l
segLength (Hori _ _ l) = l

solve :: Puzzle -> Maybe String
solve (Puzzle words pts) =
  if not (earlyCheck words segs) || null final_pairs
    then Nothing
    else Just (tomat final_pairs)
  where
    segs = genSegs pts
    nsegs = length segs
    arr_segs = listArray (0, nsegs -1) segs
    nwords = length words
    arr_words = listArray (0, nwords -1) words
    word_and_seg_pair_ids =
      [ (wi, si) | wi <- [0 .. (nwords -1)], si <- [0 .. (nsegs -1)], segLength (arr_segs ! si) == length (arr_words ! wi)
      ]
    fit (w0, s0) (w1, s1) = norep && inter && same_c
      where
        norep = w0 /= w1 && s0 /= s1
        seg0 = arr_segs ! s0
        seg1 = arr_segs ! s1
        inter = hasIntersected seg0 seg1
        word0 = arr_words ! w0
        word1 = arr_words ! w1
        (p0, p1) = wordPos seg0 seg1
        same_c = (word0 !! p0) == (word1 !! p1)
    pairs2GroupList xs = rs'
      where
        xs' = sortOn snd xs
        xs'' = groupBy (\a b -> snd a == snd b) xs'
        rs = map (\x -> (,) ((snd . head) x) (map fst x)) xs''
        rs' = map (second IntSet.fromList) rs
    go_constrain [] allow = allow
    go_constrain cons [] = []
    go_constrain c@((i1, s1) : cons) a@((i2, s2) : allow)
      | i1 < i2 = go_constrain cons a
      | i1 > i2 = (i2, s2) : go_constrain c allow
      | otherwise = (i2, IntSet.intersection s1 s2) : go_constrain cons allow

    init_allowed = pairs2GroupList word_and_seg_pair_ids
    constrains_arr = [(p0, pairs2GroupList [p1 | p1 <- word_and_seg_pair_ids, fit p0 p1]) | p0 <- word_and_seg_pair_ids]
    constrains = Map.fromList constrains_arr

    find sels [] = sels
    find sels allows = case dropWhile null anses of
      [] -> []
      (ans : _) -> ans
      where
        a = trace "sels" sels
        b = trace "allows" allows
        gsize = IntSet.size . snd
        (seg_i, candidates) = minimumBy (\a b -> compare (gsize a) (gsize b)) allows
        allows' = filter ((/= seg_i) . fst) allows
        candidates' = IntSet.elems candidates
        gfilter word_i = case Map.lookup (word_i, seg_i) constrains of
          Nothing -> allows''
          Just cons -> go_constrain cons allows''
          where
            allows'' = map (second (IntSet.delete word_i)) allows'
        anses = map (\i -> find ((i, seg_i) : sels) (gfilter i)) candidates'
    final_pairs = find [] init_allowed

    pairs2ans ps = nub $ pairs2ans' ps

    pairs2ans' [] = []
    pairs2ans' ((word_i, seg_i) : ps) = res ++ pairs2ans' ps
      where
        word = arr_words ! word_i
        res = case arr_segs ! seg_i of
          Vert r c l -> zipWith (\r' ch -> ((r', c), ch)) [r .. (r + l -1)] word
          Hori r c l -> zipWith (\c' ch -> ((r, c'), ch)) [c .. (c + l -1)] word

    tomat ps = unlines qq
      where
        pp = pairs2ans ps
        pmap = Map.fromList pp
        n = maximum $ map (fst . fst) pp
        m = maximum $ map (snd . fst) pp
        qq =
          [ [ Data.Maybe.fromMaybe ' ' (Map.lookup (r, c) pmap)
              | c <- [1 .. m]
            ]
            | r <- [1 .. n]
          ]

main :: IO ()
main = do
  s <- readFile "~/Downloads/p7_09d.dat"
  let res = solve $ readCrossword s
  putStr $ Data.Maybe.fromJust res
