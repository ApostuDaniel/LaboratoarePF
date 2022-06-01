--Ex. 1
data Tree = Leaf Int | Node Int Tree Tree deriving (Show, Eq)

t1 = Node 1 t2 t3

t1' = Node 1 t2 t3'

t2 = Node 2 (Leaf 4) (Leaf 5)

t2' = t2

t3 = Node 3 (Leaf 6) (Leaf 7)

t3' = Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9))

data Dir = L | R

type Pos = [Dir] -- sinonim de tip: Pos e o lista de Dir

elemAt :: Pos -> Tree -> Tree
elemAt [] t = t
elemAt (L : pos) (Node _ l r) = elemAt pos l
elemAt (R : pos) (Node _ l r) = elemAt pos r

changeAt :: Pos -> Tree -> Int -> Tree
changeAt [] (Leaf _) i = Leaf i
changeAt [] (Node _ l r) i = Node i l r
changeAt (L : pos) (Node v l r) i = Node v (changeAt pos l i) r
changeAt (R : pos) (Node v l r) i = Node v l (changeAt pos r i)

data Crumb = LL Int Tree | RR Int Tree deriving (Show, Eq)

type Trail = [Crumb]

type Zipper = (Trail, Tree) -- reprezinta un arbore cu un nod
-- fixat ca nod curent

change :: Zipper -> Int -> Maybe Zipper
change (trail, Leaf _) i = Just (trail, Leaf i)
change (trail, Node _ l r) i = Just (trail, Node i l r)

goLeft :: Zipper -> Maybe Zipper
goLeft (trail, Node v l r) = Just ((LL v r : trail), l)
goLeft (trail, Leaf _) = Nothing

goRight :: Zipper -> Maybe Zipper
goRight (trail, Node v l r) = Just ((RR v l : trail), r)
goRight (trail, Leaf _) = Nothing

goUp :: Zipper -> Maybe Zipper
goUp (LL v r : trail, t) = Just (trail, Node v t r)
goUp (RR v l : trail, t) = Just (trail, Node v l t)
goUp ([], t) = Nothing

z1' = ([], t1')

change' :: Int -> Zipper -> Maybe Zipper
change' i zipper = change zipper i

--Ex 2
{-
*Main> return z1' >>= goRight >>= goLeft
Just ([LL 3 (Node 7 (Leaf 8) (Leaf 9)),RR 1 (Node 2 (Leaf 4) (Leaf 5))],Leaf 6)

*Main> return z1' >>= goRight >>= goLeft >>= change' 2
Just ([LL 3 (Node 7 (Leaf 8) (Leaf 9)),RR 1 (Node 2 (Leaf 4) (Leaf 5))],Leaf 2)

*Main> return z1' >>= goRight >>= goLeft >>= goUp
Just ([RR 1 (Node 2 (Leaf 4) (Leaf 5))],Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9)))
-}
--Ex3
data DirL = Fwd

type PosL = [DirL]

type TrailL = [Int]

type ZipperL = (TrailL, [Int])

-- invariant: (trail, list) reprezinta lista (reverse trail) ++ list

zipperAtAuxL :: PosL -> [Int] -> TrailL -> ZipperL
zipperAtAuxL [] l trail = (trail, l)
zipperAtAuxL (Fwd : pos) (h : t) trail = zipperAtAuxL pos t (h : trail)

zipperAtL :: PosL -> [Int] -> ZipperL
zipperAtL p l = zipperAtAuxL p l []

changeL :: ZipperL -> Int -> Maybe ZipperL
changeL (trail, h : t) v = Just (trail, v : t)
changeL (trail, []) v = Nothing

goFwd' :: ZipperL -> Maybe ZipperL
goFwd' (trail, h : t) = Just (h : trail, t)
goFwd' (trail, []) = Nothing

goBwd' :: ZipperL -> Maybe ZipperL
goBwd' (v : trail, l) = Just (trail, v : l)
goBwd' ([], l) = Nothing

changeL' :: Int -> ZipperL -> Maybe ZipperL
changeL' v zipper = changeL zipper v

a = zipperAtL [] [4, 5, 1, 3, 2, 6]

--Ex 4
{-
*Main> return a >>= goFwd' >>= goFwd' >>= goFwd' >>= goBwd'
Just ([5,4],[1,3,2,6])

*Main> return a >>= goFwd' >>= goFwd' >>= goFwd' >>= goBwd' >>= changeL' 15
Just ([5,4],[15,3,2,6])
-}

--Ex 5
data BigTree = BigLeaf Int | BigNode Int [BigTree] deriving (Show, Eq)

data TreeCrumb = TCrumb Int [BigTree] [BigTree] deriving (Show, Eq)

type TreeZipper = (BigTree, [TreeCrumb])

treeUp :: TreeZipper -> Maybe TreeZipper
treeUp (node, (TCrumb val ls rs) : rest) = Just (BigNode val (ls ++ [node] ++ rs), rest)
treeUp (node, []) = Nothing

--Coboram la al x-ulea nod din lista de copii directi
goXthNode :: Int -> TreeZipper -> Maybe TreeZipper
goXthNode x (BigLeaf _, _) = Nothing
goXthNode x (BigNode val children, trail)
  | x > length children = Nothing
  | x < 1 = Nothing
  | otherwise = Just (hd, TCrumb val before tail : trail)
  where
    (before, hd : tail) = splitAt (x - 1) children

changeTreeValue :: Int -> TreeZipper -> Maybe TreeZipper
changeTreeValue x (BigNode val children, trail) = Just (BigNode x children, trail)
changeTreeValue x (BigLeaf val, trail) = Just (BigLeaf x, trail)

n2 = BigNode 2 [BigLeaf 5, BigLeaf 6, BigLeaf 7]

n3 = BigNode 3 [BigLeaf 8, BigLeaf 9, BigLeaf 10]

n4 = BigNode 4 [BigLeaf 11, BigLeaf 12, BigLeaf 13]

n1 = BigNode 1 [n2, n3, n4]

n1Zipper = (n1, [])

{-
*Main> return n1Zipper >>= goXthNode 1 >>= goXthNode 2
Just (BigLeaf 6,[TCrumb 2 [BigLeaf 5] [BigLeaf 7],TCrumb 1 [] [BigNode 3 [BigLeaf 8,BigLeaf 9,BigLeaf 10],BigNode 4 [BigLeaf 11,BigLeaf 12,BigLeaf 13]]])

*Main> return n1Zipper >>= goXthNode 1 >>= goXthNode 3 >>= changeTreeValue 75
Just (BigLeaf 75,[TCrumb 2 [BigLeaf 5,BigLeaf 6] [],TCrumb 1 [] [BigNode 3 [BigLeaf 8,BigLeaf 9,BigLeaf 10],BigNode 4 [BigLeaf
11,BigLeaf 12,BigLeaf 13]]])
-}
