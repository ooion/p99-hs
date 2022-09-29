data Tree a = Node a [Tree a]
  deriving (Eq, Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

-- 70C
nnodes :: Tree a -> Int
nnodes (Node _ children) = foldl upd 1 children
  where
    upd n t = n + nnodes t

-- 70
treeToString :: Tree Char -> String
treeToString (Node x []) = x : "^"
treeToString (Node x cs) = (x : concatMap treeToString cs) ++ "^"

stringToTree :: String -> Tree Char
stringToTree xs = fst $ go xs
  where
    go (x : '^' : xs) = (Node x [], xs)
    go (x : xs) = let (cs, xs') = goc xs in (Node x cs, xs')
    go [] = error "parse error"
    goc ('^' : xs) = ([], xs)
    goc xs =
      let (c, xs') = go xs
          (cs, xs'') = goc xs'
       in (c : cs, xs'')

-- 71
ipl :: Tree a -> Int
ipl = ipl' 1
  where
    ipl' h (Node _ cs) =
      h * length cs + sum (map (ipl' (h + 1)) cs)

-- 72
bottomUp :: Tree Char -> String
bottomUp (Node x []) = [x]
bottomUp (Node x cs) = concatMap bottomUp cs ++ [x]

-- 73
lisp :: Tree Char -> String
lisp (Node x []) = [x]
lisp (Node x cs) = ('(' : [x]) ++ content' ++ ")"
  where
    content = unwords $ map lisp cs
    content' = if null content then content else ' ' : content
