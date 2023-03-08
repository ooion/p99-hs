{-# LANGUAGE TupleSections #-}

import Control.Arrow (ArrowLoop (loop))
import Control.Monad
import Control.Monad.ST.Lazy
import Data.Array (listArray)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (groupBy, intercalate, minimumBy, nub, sort)
import Data.STRef.Lazy
import Debug.Trace (trace)

debug s x = trace (s ++ show x) x

data DNode s
  = DNode
      { left, right, up, down :: STRef s (DNode s),
        row, col :: Int
      }
  | Nil

instance Eq (DNode s) where
  Nil == Nil = True
  Nil == DNode {} = False
  DNode {} == Nil = False
  a == b = row a == row b && col a == col b

newNode l r u d ro co =
  do
    l' <- newSTRef l
    r' <- newSTRef r
    u' <- newSTRef u
    d' <- newSTRef d
    return (DNode l' r' u' d' ro co)

fixNode node =
  let f dir = getDir dir node >>= \t -> when (t == Nil) (setDir dir node node)
   in do mapM_ f [up, down, left, right] >> pure node

getDir dir node = readSTRef (dir node)

setDir dir node = writeSTRef (dir node)

data DGlobal s = DGlobal
  { head :: DNode s,
    row_heads, col_heads :: STArray s Int (DNode s),
    col_sizes :: STArray s Int Int
  }

build nrow ncol = do
  head <- newNode Nil Nil Nil Nil (-1) (-1) >>= fixNode
  row_heads <- newArray (0, nrow - 1) Nil :: ST s (STArray s Int (DNode s))
  col_heads <- newArray (0, ncol - 1) Nil :: ST s (STArray s Int (DNode s))
  col_sizes <- newArray (0, ncol - 1) 0
  forM_ [0.. ncol - 1] $ \i -> do
    h0 <- getDir left head
    node <- newNode h0 head Nil Nil (-1) i >>= fixNode
    setDir right h0 node >> setDir left head node
    writeArray col_heads i node
  pure $ DGlobal head row_heads col_heads col_sizes

addColSize :: Int -> DGlobal s -> Int -> ST s ()
addColSize dt g c = do
  sz <- readArray (col_sizes g) c
  writeArray (col_sizes g) c (sz + dt)

incColSize = addColSize 1

decColSize = addColSize (-1)

addLink :: DGlobal s -> Int -> Int -> ST s ()
addLink g r c = do
  incColSize g c
  hu <- readArray (col_heads g) c
  hd <- getDir down hu
  hr <- readArray (row_heads g) r
  node <- newNode Nil Nil hu hd r c
  setDir down hu node
  setDir up hd node
  if hr == Nil
    then fixNode node >>= writeArray (row_heads g) r
    else
      setDir right node hr
        >> getDir left hr >>= \hl ->
          setDir left node hl
            >> setDir left hr node
            >> setDir right hl node

-- unLinkUD:: DGlobal s -> DNode s -> ST s ()
unLink d1 d2 g node = do
  n1 <- getDir d1 node
  n2 <- getDir d2 node
  setDir d1 n2 n1
  setDir d2 n1 n2
  pure node

reLink d1 d2 g node = do
  n1 <- getDir d1 node
  n2 <- getDir d2 node
  setDir d1 n2 node
  setDir d2 n1 node
  pure node

unLinkUD g node = decColSize g (col node) >> unLink up down g node

reLinkUD g node = incColSize g (col node) >> reLink up down g node

unLinkLR = unLink left right

reLinkLR = reLink left right

dloop dir func st = loop st
  where
    loop a = do
      a' <- getDir dir a
      if a' /= st
        then func a' >>= \b -> loop a' >>= \bs -> pure (b : bs)
        else pure []

dloop1 dir func = let func' a = func a >> pure () in dloop dir func'

removeCol g c = readArray (col_heads g) c >>= unLinkLR g >>= dloop1 down (dloop1 right (unLinkUD g))

resumeCol g c = readArray (col_heads g) c >>= reLinkLR g >>= dloop1 up (dloop1 left (reLinkUD g))

search :: DGlobal s -> [Int] -> ST s [[Int]]
search g selects = do
  cols <- dloop right (pure . col) (Main.head g)
  if null cols
    then pure [selects]
    else do
      sizes <- mapM (readArray (col_sizes g)) cols
      let c = snd $ minimumBy (\a b -> compare (fst a) (fst b)) (zip sizes cols)
      nd <- readArray (col_heads g) c
      let find x =
            removeCol g c
              >> dloop1 right (removeCol g . col) x
              >> search g (row x : selects)
              >>= \sols ->
                dloop1 left (resumeCol g . col) x
                  >> resumeCol g c
                  >> pure sols
      solss <- dloop down find nd
      pure $ concat solss

sudoku xs = if has_conflict then [] else ans
  where
    ncol = 81 * 4 -- Grid, Occupy, row and column
    nrow = 81 * 9 -- Grid x [1..9]
    selected_rows = map (\(n, i) -> i * 9 + n - 1) $ filter ((/= 0) . fst) $ zip (concat xs) [0 ..]
    selected_cols = nub $ concatMap get_cols_from_row selected_rows
    has_conflict = 4 * length selected_rows /= length selected_cols
    cgrid (r, c, num) = let rr = div r 3; cc = div c 3 in (rr * 3 + cc) * 9 + num
    coccu (r, c, num) = 81 + r * 9 + c
    crow (r, c, num) = 81 * 2 + r * 9 + num
    ccol (r, c, num) = 81 * 3 + c * 9 + num
    id2rcn i = (div i 81, div (rem i 81) 9, rem i 9)
    get_cols_from_row rid = let rcn = id2rcn rid in [cgrid rcn, coccu rcn, crow rcn, ccol rcn]
    links = concatMap (\i -> map (i,) (get_cols_from_row i)) [0 .. nrow -1]

    dlx = do
      g <- build nrow ncol
      mapM_ (uncurry (addLink g)) links
      mapM_ (removeCol g) selected_cols
      search g selected_rows

    sol2mat rows = map (map ((+ 1) . (`rem` 9))) $ groupBy (\a b -> div a 81 == div b 81) $ sort rows
    ans = map sol2mat $ runST dlx

main = do
  puzzle_strs <- replicateM 9 getLine
  let prob = map (map digitToInt) puzzle_strs
  forM_ (sudoku prob) $ \sol -> do
    putStrLn $ intercalate "\n" (map (unwords . map show) sol) ++ "\n"
