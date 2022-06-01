--Ex. 1.1
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--Ex. 2.1
applyFunctionAndSum :: (Int -> Int) -> Int -> Int -> Int
applyFunctionAndSum f start end = applyFunctionAndSum' f start end 0

applyFunctionAndSum' :: (Int -> Int) -> Int -> Int -> Int -> Int
applyFunctionAndSum' f start end a
  | start < end = applyFunctionAndSum' f (start + 1) end (a + f start)
  | start == end = a + f start
  | otherwise = error "Interval incorectly defined"

--Ex. 2.2
comp' :: (t1 -> t2) -> (t3 -> t1) -> (t3 -> t2)
comp' f g = \x -> f (g x)

--Ex. 2.3

compList :: [t1 -> t1] -> (t1 -> t1)
compList [] = error "Can't make composition from 0 functions"
compList (hd : tail)
  | null tail = hd
  | otherwise = comp' hd (compList tail)

--Ex. 2.4
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (hd : tail) = hd + (sum' tail)

--Ex. 2.5
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (hd : tail) = (f hd) : map' f tail

--Ex. 2.6
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (hd : tail)
  | f hd = hd : filter' f tail
  | otherwise = filter' f tail

--Ex. 2.7
data MyList = Void | Item Integer MyList deriving (Show, Eq)

foldr' :: (Integer -> Integer -> Integer) -> Integer -> MyList -> Integer
foldr' f a Void = a
foldr' f a (Item hd tail) = f hd (foldr' f a tail)

foldl' :: (Integer -> Integer -> Integer) -> Integer -> MyList -> Integer
foldl' f a Void = a
foldl' f a (Item hd tail) = foldl' f a' tail where a' = f a hd

--Ex. 2.8
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

preOrder :: Arb -> (Integer -> Integer) -> [Integer]
preOrder Frunza f = []
preOrder (Nod val st dr) f = f val : preOrder st f ++ preOrder dr f

inOrder :: Arb -> (Integer -> Integer) -> [Integer]
inOrder Frunza f = []
inOrder (Nod val st dr) f = inOrder st f ++ (f val : inOrder dr f)

postOrder :: Arb -> (Integer -> Integer) -> [Integer]
postOrder Frunza f = []
postOrder (Nod val st dr) f = postOrder st f ++ postOrder dr f ++ [f val]

--Ex. 2.9
parcurgere :: Arb -> (Arb -> (Integer -> Integer) -> [Integer]) -> (Integer -> Integer) -> [Integer]
parcurgere Frunza _ _ = []
parcurgere radacina order f = order radacina f

--Ex. 3.1
quicksort :: [a] -> (a -> a -> Bool) -> [a]
quicksort [] _ = []
quicksort (hd : tail) p = quicksort (filter (not . p hd) tail) p ++ [hd] ++ quicksort (filter (p hd) tail) p

--Ex. 3.2
data Either' a b = Left' a | Right' b deriving (Show, Eq)

either' :: (a -> c) -> (b -> c) -> Either' a b -> c
either' f g (Left' l) = f l
either' f g (Right' r) = g r

lefts' :: [Either' a b] -> [a]
lefts' [] = []
lefts' ((Right' r) : tail) = lefts' tail
lefts' ((Left' l) : tail) = l : lefts' tail

rights' :: [Either' a b] -> [b]
rights' [] = []
rights' ((Right' r) : tail) = r : rights' tail
rights' ((Left' l) : tail) = rights' tail

isLeft' :: Either' a b -> Bool
isLeft' (Left' _) = True
isLeft' (Right' _) = False

isRight' :: Either' a b -> Bool
isRight' (Left' _) = False
isRight' (Right' _) = True

fromLeft' :: a -> Either' a b -> a
fromLeft' p (Right' _) = p
fromLeft' _ (Left' l) = l

fromRight' :: b -> Either' a b -> b
fromRight' _ (Right' r) = r
fromRight' p (Left' _) = p

partitionEithers' :: [Either' a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' list = (lefts' list, rights' list)

--Ex. 3.3

data (Ord a) => Arb' a = Frunza' | Nod' a (Arb' a) deriving (Show, Eq)

--Ex 3.4