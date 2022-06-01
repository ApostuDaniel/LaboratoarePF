--Ex 0.1
{-
Prelude> :i Show
type Show :: * -> Constraint
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
        -- Defined in `GHC.Show'
-}
{-
Prelude> :i Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
        -- Defined in `GHC.Classes'
-}

{-
Prelude> :i Ord
type Ord :: * -> Constraint
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
        -- Defined in `GHC.Classes'
-}

{-
Prelude> :i Read
type Read :: * -> Constraint
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
                             [a]
  {-# MINIMAL readsPrec | readPrec #-}
        -- Defined in `GHC.Read'
-}

{-
Prelude> :i Enum
type Enum :: * -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
        -- Defined in `GHC.Enum'
-}

{-
Prelude> :i Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
        -- Defined in `GHC.Num'
-}

--Ex. 0.2
{-
instance Show a => Show [a] -- Defined in `GHC.Show'
instance Show Word -- Defined in `GHC.Show'
instance Show GHC.Types.RuntimeRep -- Defined in `GHC.Show'
instance Show Ordering -- Defined in `GHC.Show'
instance Show a => Show (Maybe a) -- Defined in `GHC.Show'
instance Show Integer -- Defined in `GHC.Show'
instance Show Int -- Defined in `GHC.Show'
instance Show Char -- Defined in `GHC.Show'
instance Show Bool -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k) =>
         Show (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j) =>
         Show (a, b, c, d, e, f, g, h, i, j)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i) =>
         Show (a, b, c, d, e, f, g, h, i)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h) =>
         Show (a, b, c, d, e, f, g, h)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g) =>
         Show (a, b, c, d, e, f, g)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e, Show f) =>
         Show (a, b, c, d, e, f)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d, Show e) =>
         Show (a, b, c, d, e)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d)
  -- Defined in `GHC.Show'
instance (Show a, Show b, Show c) => Show (a, b, c)
  -- Defined in `GHC.Show'
instance (Show a, Show b) => Show (a, b) -- Defined in `GHC.Show'
instance Show () -- Defined in `GHC.Show'
instance (Show a, Show b) => Show (Either a b)
  -- Defined in `Data.Either'
instance Show Float -- Defined in `GHC.Float'
instance Show Double -- Defined in `GHC.Float'
-}

{-
instance Eq a => Eq [a] -- Defined in `GHC.Classes'
instance Eq Word -- Defined in `GHC.Classes'
instance Eq Ordering -- Defined in `GHC.Classes'
instance Eq Int -- Defined in `GHC.Classes'
instance Eq Float -- Defined in `GHC.Classes'
instance Eq Double -- Defined in `GHC.Classes'
instance Eq Char -- Defined in `GHC.Classes'
instance Eq Bool -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m, Eq n) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j) =>
         Eq (a, b, c, d, e, f, g, h, i, j)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
         Eq (a, b, c, d, e, f, g, h, i)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
         Eq (a, b, c, d, e, f, g, h)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
         Eq (a, b, c, d, e, f, g)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
         Eq (a, b, c, d, e, f)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
  -- Defined in `GHC.Classes'
instance (Eq a, Eq b) => Eq (a, b) -- Defined in `GHC.Classes'
instance Eq () -- Defined in `GHC.Classes'
instance Eq a => Eq (Maybe a) -- Defined in `GHC.Maybe'
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in `Data.Either'
instance Eq Integer
  -- Defined in `integer-gmp-1.0.3.0:GHC.Integer.Type'
-}

{-
instance Ord a => Ord [a] -- Defined in `GHC.Classes'
instance Ord Word -- Defined in `GHC.Classes'
instance Ord Ordering -- Defined in `GHC.Classes'
instance Ord Int -- Defined in `GHC.Classes'
instance Ord Float -- Defined in `GHC.Classes'
instance Ord Double -- Defined in `GHC.Classes'
instance Ord Char -- Defined in `GHC.Classes'
instance Ord Bool -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) =>
         Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) =>
         Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j, Ord k, Ord l, Ord m) =>
         Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j, Ord k, Ord l) =>
         Ord (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j, Ord k) =>
         Ord (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i, Ord j) =>
         Ord (a, b, c, d, e, f, g, h, i, j)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
          Ord i) =>
         Ord (a, b, c, d, e, f, g, h, i)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
          Ord h) =>
         Ord (a, b, c, d, e, f, g, h)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) =>
         Ord (a, b, c, d, e, f, g)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) =>
         Ord (a, b, c, d, e, f)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
  -- Defined in `GHC.Classes'
instance (Ord a, Ord b) => Ord (a, b) -- Defined in `GHC.Classes'
instance Ord () -- Defined in `GHC.Classes'
instance Ord a => Ord (Maybe a) -- Defined in `GHC.Maybe'
instance (Ord a, Ord b) => Ord (Either a b)
  -- Defined in `Data.Either'
instance Ord Integer
  -- Defined in `integer-gmp-1.0.3.0:GHC.Integer.Type'
-}

{-
instance Read a => Read [a] -- Defined in `GHC.Read'
instance Read Word -- Defined in `GHC.Read'
instance Read Ordering -- Defined in `GHC.Read'
instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
instance Read Integer -- Defined in `GHC.Read'
instance Read Int -- Defined in `GHC.Read'
instance Read Float -- Defined in `GHC.Read'
instance Read Double -- Defined in `GHC.Read'
instance Read Char -- Defined in `GHC.Read'
instance Read Bool -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m, Read n, Read o) =>
         Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m, Read n) =>
         Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m) =>
         Read (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l) =>
         Read (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k) =>
         Read (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j) =>
         Read (a, b, c, d, e, f, g, h, i, j)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i) =>
         Read (a, b, c, d, e, f, g, h, i)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h) =>
         Read (a, b, c, d, e, f, g, h)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f,
          Read g) =>
         Read (a, b, c, d, e, f, g)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e, Read f) =>
         Read (a, b, c, d, e, f)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d, Read e) =>
         Read (a, b, c, d, e)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d)
  -- Defined in `GHC.Read'
instance (Read a, Read b, Read c) => Read (a, b, c)
  -- Defined in `GHC.Read'
instance (Read a, Read b) => Read (a, b) -- Defined in `GHC.Read'
instance Read () -- Defined in `GHC.Read'
instance (Read a, Read b) => Read (Either a b)
  -- Defined in `Data.Either'
-}

{-
instance Enum Word -- Defined in `GHC.Enum'
instance Enum Ordering -- Defined in `GHC.Enum'
instance Enum Integer -- Defined in `GHC.Enum'
instance Enum Int -- Defined in `GHC.Enum'
instance Enum Char -- Defined in `GHC.Enum'
instance Enum Bool -- Defined in `GHC.Enum'
instance Enum () -- Defined in `GHC.Enum'
instance Enum Float -- Defined in `GHC.Float'
instance Enum Double -- Defined in `GHC.Float'
-}

{-
instance Num Word -- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
instance Num Float -- Defined in `GHC.Float'
instance Num Double -- Defined in `GHC.Float'
-}

--Ex. 0.3

data Nat = Cons [Bool]

--Ex. 0.4

simpl :: [Bool] -> [Bool]
simpl [] = []
simpl (hd : tail)
  | not hd = simpl tail
  | otherwise = hd : tail

instance Eq Nat where
  Cons a == Cons b = simpl a == simpl b

{-
*Main> (Cons [True, False]) == (Cons [True, False])
True
*Main> (Cons [False, False]) == (Cons [True, False])
False
*Main> (Cons [False, True]) == (Cons [True])
True
*Main> (Cons [True, True]) == (Cons [True])
False
*Main> (Cons [True]) == (Cons [True])
True
*Main> (Cons []) == (Cons [])
True
-}

instance Ord Nat where
  Cons a <= Cons b = simpl a <= simpl b

toInt :: [Bool] -> Integer
toInt l = toInt' r 0 where r = reverse (simpl l)

toInt' :: [Bool] -> Integer -> Integer
toInt' [] _ = 0
toInt' (hd : tail) p
  | hd = 2 ^ p + toInt' tail (p + 1)
  | otherwise = toInt' tail (p + 1)

toBoolArr :: Integer -> [Bool]
toBoolArr i
  | i < 0 = error "Cant make natural number from negative integer"
  | i == 0 = []
  | otherwise = (rem i 2 == 1) : toBoolArr (quot i 2)

instance Enum Nat where
  toEnum i = Cons (toBoolArr (toInteger i))
  fromEnum (Cons n) = fromInteger (toInt n)

instance Real Nat where
  toRational n = toRational (toInteger n)

instance Integral Nat where
  toInteger (Cons n) = toInt n
  quotRem (Cons a) (Cons b) = (quotient, reminder)
    where
      quotient = Cons (toBoolArr (toInt a `quot` toInt b))
      reminder = Cons (toBoolArr (toInt a `rem` toInt b))

{-
addWithCary :: [Bool] -> [Bool] -> Bool -> [Bool]
addWithCary [] [] x = [x]
addWithCary
addWithCary (hd:tail) x
    | hd && x =
-}

instance Num Nat where
  fromInteger i = Cons (toBoolArr i)
  (Cons a) + (Cons b) = Cons sum where sum = toBoolArr (toInt a + toInt b)
  (Cons a) - (Cons b) = Cons dif where dif = toBoolArr (toInt a - toInt b)
  (Cons a) * (Cons b) = Cons mul where mul = toBoolArr (toInt a * toInt b)
  abs n = n
  negate (Cons []) = Cons [False]
  negate n = Cons []
  signum (Cons []) = Cons []
  signum n = Cons [False]

--Ex. 0.5
data Complex a = Complex a a

instance (Num a, Floating a) => Num (Complex a) where
  (Complex a b) + (Complex x y) = Complex (a + x) (b + y)
  (Complex a b) * (Complex x y) = Complex (a * x - b * y) (b * x + a * y)
  (Complex a b) - (Complex x y) = Complex (a - x) (b - y)
  fromInteger i = Complex (fromInteger i) 0
  abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0
  signum a = complexDivision a (abs a)

complexDivision :: (Num a, Floating a) => Complex a -> Complex a -> Complex a
complexDivision (Complex a b) (Complex c d) = Complex x y
  where
    x = (a * c + b * d) / (c * c + d * d)
    y = (b * c - a * d) / (c * c + d * d)

--Ex. 0.6

class (Eq a) => MyOrd a where
  compare' :: a -> a -> Ordering
  compare' x y
    | leq x y && x /= y = LT
    | x == y = EQ
    | otherwise = GT
  lt :: a -> a -> Bool
  lt x y = leq x y && x /= y
  leq :: a -> a -> Bool
  leq x y = comparison == EQ || comparison == LT where comparison = compare' x y
  gt :: a -> a -> Bool
  gt x y = not (leq x y)
  geq :: a -> a -> Bool
  geq x y = not (leq x y) || x == y
  max' :: a -> a -> a
  max' x y
    | leq x y = y
    | otherwise = x
  min' :: a -> a -> a
  min' x y
    | not (leq x y) || x == y = y
    | otherwise = x

instance MyOrd Int where
  leq x y = x <= y

id' :: Int -> Int
id' a = a

{-
*Main> leq (4 :: Int) (5 :: Int)
True
*Main> geq (4 :: Int) (5 :: Int)
False
*Main> compare' (4 :: Int) (5 :: Int)
LT
-}

instance MyOrd a => MyOrd [a] where
  leq [] a = True
  leq a [] = False
  leq (hd1 : tail1) (hd2 : tail2)
    | leq hd1 hd2 = leq tail1 tail2
    | otherwise = False

sort :: MyOrd a => [a] -> [a]
sort [] = []
sort (hd : tail) = sort (filter (not . leq hd) tail) ++ [hd] ++ sort (filter (leq hd) tail)

{-
*Main> let x = [(4 :: Int), (3 :: Int), (6 :: Int), (2 :: Int)]
*Main> sort x
[2,3,4,6]
-}

--Ex 0.7
data Nat' = Zero | Succ Nat' deriving (Eq)

--Show
{-
*Main> let x = Zero
*Main> x
Zero
*Main> Succ ( Succ (Zero))
Succ (Succ Zero)
*Main> (Succ (Succ (Zero)))
Succ (Succ Zero)
-}
--Show printeaza la fel ca in definitie

--Eq
{-
*Main> Zero == Zero
True
*Main> Zero == (Zero)
True
*Main> Zero == (Succ Zero)
False
*Main> Succ Zero == Succ Zero
True
*Main> Succ Succ Zero == Succ Zero
*Main> Succ (Succ Zero) == Succ Zero
False
-}
--Eq compara pina cind gaseste constructori care nu sunt egali

--Ord
{-
*Main> Succ (Succ Zero) == Succ Zero
False
*Main> Succ Zero <= Succ (Succ Zero)
True
*Main> Succ Zero < Succ (Succ Zero)
True
*Main> Succ Zero > Succ (Succ Zero)
False
*Main> Succ Zero >= Succ (Succ Zero)
False
*Main> max (Succ Zero) (Succ (Succ Zero))
Succ (Succ Zero)
*Main> min (Succ Zero) (Succ (Succ Zero))
Succ Zero
*Main> compare (Succ Zero) (Succ (Succ Zero))
LT
-}
--Ord ordoneaza dupa ordinea construcotrilor in definitie

--Ex. 0.8
instance Show Nat' where
  show Zero = "o"
  show (Succ x) = "s" ++ show x

{-
*Main> (Succ (Succ Zero))
sso
*Main> Zero
o
-}

--Ex. 0.9
instance Ord Nat' where
  Zero <= Zero = True
  Zero <= (Succ x) = True
  (Succ x) <= Zero = False
  (Succ x) <= (Succ y) = x <= y

--Ex. 0.10
data Arb = Frunza | Nod Integer Arb Arb deriving (Eq)

instance Show Arb where
  show Frunza = "()"
  show (Nod x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

{-
*Main> Nod 2 (Nod 3 Frunza Frunza) (Nod 4 Frunza Frunza)
(2(3()())(4()()))
-}

--Ex. 0.11
data Arb' a = Leaf | Node a (Arb' a) (Arb' a)

instance (Show a) => Show (Arb' a) where
  show Leaf = "()"
  show (Node x l r) = "(" ++ show x ++ show l ++ show r ++ ")"

{-
*Main> Node 2.1 (Node 3.1 Leaf Leaf) (Node 4.1 Leaf Leaf)
(2.1(3.1()())(4.1()()))
-}
-- a trebuie sa faca parte din clasa show pentru a putea reprezenta cu show valorile din noduri

--Ex. 0.12
data Nat'' = Zero' | Succ' Nat''

instance Eq Nat'' where
  Zero' == Zero' = True
  Zero' == Succ' x = False
  Succ' x == Zero' = False
  Succ' x == Succ' y = x == y

--Ex. 0.13

instance (Eq a) => Eq (Arb' a) where
  Leaf == Leaf = True
  Leaf == Node {} = False
  Node {} == Leaf = False
  (Node x1 l1 r1) == (Node x2 l2 r2) = x1 == x2 && l1 == l2 && r1 == r2

--Ex. 0.14
class Pretty a where
  prettyPrint :: a -> String

instance Pretty Nat' where
  prettyPrint Zero = "o"
  prettyPrint (Succ x) = "s" ++ prettyPrint x

instance (Show a) => Pretty (Arb' a) where
  prettyPrint Leaf = "()"
  prettyPrint (Node x l r) = "(" ++ show x ++ prettyPrint l ++ prettyPrint r ++ ")"

--Ex. 0.15
class MyNum a where
  myToInt :: a -> Int

instance MyNum Nat' where
  myToInt Zero = 0
  myToInt (Succ x) = 1 + myToInt x

--Ex. 0.16
data Nat1 = Zero1 | Succ1 Nat1 deriving (Show, Eq)

instance Num Nat1 where
  fromInteger 0 = Zero1
  fromInteger x = Succ1 (fromInteger x - 1)
  Zero1 + Zero1 = Zero1
  Zero1 + (Succ1 x) = Succ1 x
  (Succ1 x) + Zero1 = Succ1 x
  (Succ1 x) + (Succ1 y) = Succ1 (x + Succ1 y)
  Zero1 * Zero1 = Zero1
  Zero1 * Succ1 x = Zero1
  Succ1 x * Zero1 = Zero1
  (Succ1 x) * (Succ1 y)
    | x == Zero1 = y
    | y == Zero1 = x
    | otherwise = Succ1 x + (Succ1 x) * y
  Zero1 - Zero1 = Zero1
  Zero1 - (Succ1 x) = Zero1
  (Succ1 x) - Zero1 = Succ1 x
  (Succ1 x) - (Succ1 y) = x - y
  abs x = x
  signum Zero1 = Zero1
  signum (Succ1 x) = Succ1 Zero1

--Ex. 0.17
data List a = Nil | Cons' a (List a)

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  Nil == (Cons' _ _) = False
  (Cons' _ _) == Nil = False
  (Cons' hd1 tail1) == (Cons' hd2 tail2) = hd1 == hd2 && tail1 == tail2

--Ex. 0.18

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons' hd tail) = Cons' (f hd) (fmap f tail)