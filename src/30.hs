{-# LANGUAGE LambdaCase #-}

import Control.Applicative
-- import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List
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
