import System.Win32 (COORD (x))

--Ex. 1.2

--data MobileDevice = Smartphone | Laptop | Tablet

{-
*Main> :t Smartphone
Smartphone :: MobileDevice
*Main> :t Laptop
Laptop :: MobileDevice
*Main> :t Tablet
Tablet :: MobileDevice
-}
--Ex. 1.3

data Culori = Gri | Negru | Rosu | Alb deriving (Show)

data MobileDevice = Smartphone Culori | Laptop Culori | Tablet Culori

{-
*Main> :t (Smartphone Gri)
(Smartphone Gri) :: MobileDevice
*Main> : (Laptop Rosu)
(Laptop Rosu) :: MobileDevice
*Main> :t (Tablet Negru)
(Tablet Negru) :: MobileDevice
-}

--Ex. 1.4

afisCuloare :: Culori -> String
afisCuloare Gri = "Culoarea e gri"
afisCuloare Negru = "Cuolarea e negru"
afisCuloare Rosu = "Culoarea e rosu"
afisCuloare Alb = "Culoarea e alb"

culoareMobileDevice :: MobileDevice -> String
culoareMobileDevice (Smartphone color) = afisCuloare color
culoareMobileDevice (Laptop color) = afisCuloare color
culoareMobileDevice (Tablet color) = afisCuloare color

{-
*Main> culoareMobileDevice (Laptop Rosu)
"Culoarea e rosu"
*Main> culoareMobileDevice (Smartphone Negru)
"Cuolarea e negru"
-}

--Ex. 2.1
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

--Ex. 2.2
getNodeValue :: Arb -> Integer
getNodeValue (Nod val st dr) = val
getNodeValue Frunza = error "Frunza nu are valoare"

isFrunza :: Arb -> Bool
isFrunza Frunza = True
isFrunza (Nod _ _ _) = False

isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod value st dr) =
  (isFrunza st || value > getNodeValue st) && (isFrunza dr || value < getNodeValue dr) && isBST st && isBST dr

{-
*Main> let a = (Nod 7 (Nod 5 Frunza Frunza)(Nod 8 Frunza Frunza))
*Main> isBST a
True
-}

--Ex 2.3
search :: Arb -> Integer -> Bool
search Frunza key = False
search (Nod val st dr) key
  | val < key = search dr key
  | val > key = search st key
  | otherwise = True

{-
*Main> let a = (Nod 7 (Nod 5 Frunza Frunza)(Nod 8 Frunza Frunza))
*Main> search a 8
True
*Main> search a 6
False
-}

--Ex 2.4
insert :: Arb -> Integer -> Arb
insert Frunza key = Nod key Frunza Frunza
insert (Nod val st dr) key
  | val < key = Nod val st (insert dr key)
  | val > key = Nod val (insert st key) dr
  | otherwise = error "Binary search tree can't have duplicate values"

{-
*Main> let b = (Nod 7 (Nod 5 Frunza Frunza)(Nod 8 Frunza Frunza))
*Main> b
Nod 7 (Nod 5 Frunza Frunza) (Nod 8 Frunza Frunza)
*Main> insert b 9
Nod 7 (Nod 5 Frunza Frunza) (Nod 8 Frunza (Nod 9 Frunza Frunza))
*Main> insert b 6
Nod 7 (Nod 5 Frunza (Nod 6 Frunza Frunza)) (Nod 8 Frunza Frunza)
-}

--Ex 2.5
maxim :: Arb -> Integer
maxim Frunza = error "un arbore gol nu are un maxim"
maxim (Nod val st dr)
  | isFrunza dr = val
  | otherwise = maxim dr

minim :: Arb -> Integer
minim Frunza = error "un arbore gol nu are un minim"
minim (Nod val st dr)
  | isFrunza st = val
  | otherwise = minim st

--Ex 2.6
left :: Arb -> Arb
left Frunza = Frunza
left (Nod val st dr) = st

right :: Arb -> Arb
right Frunza = Frunza
right (Nod val st dr) = dr

removeMax :: Arb -> Arb
removeMax Frunza = error "nu pot elimina un nod din arborele gol"
removeMax (Nod val st dr)
  | isFrunza dr && isFrunza st = Frunza
  | isFrunza dr = Nod (getNodeValue st) (left st) Frunza
  | otherwise = Nod val st (removeMax dr)

--Ex 2.7

remove :: Arb -> Integer -> Arb
remove Frunza _ = error "Nodul nu exista in arbore"
remove (Nod val st dr) key
  | val == key && isFrunza st && isFrunza dr = Frunza
  | val == key && not (isFrunza st) = Nod (getNodeValue st) (left st) dr
  | val == key && not (isFrunza dr) = Nod (getNodeValue dr) st (right dr)
  | val > key && not (isFrunza st) && maxim st == key = Nod val (removeMax st) dr
  | val > key = Nod val (remove st key) dr
  | val < key && not (isFrunza dr) && maxim dr == key = Nod val st (removeMax dr)
  | otherwise = Nod val st (remove dr key)

{-
*Main> let b = (Nod 7 (Nod 5 Frunza Frunza)(Nod 8 Frunza Frunza))
*Main> remove b 6
Nod 7 (Nod 5 Frunza *** Exception: Nodul nu exista in arbore
CallStack (from HasCallStack):
  error, called at Apostu_Daniel_2A6_lab3.hs:138:19 in main:Main
*Main> remove b 8
Nod 7 (Nod 5 Frunza Frunza) Frunza
*Main> remove b 7
Nod 5 Frunza (Nod 8 Frunza Frunza)
*Main> remove b 5
Nod 7 Frunza (Nod 8 Frunza Frunza)
-}

--Ex. 2.8
preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod val st dr) = [val] ++ preOrder st ++ preOrder dr

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod val st dr) = inOrder st ++ [val] ++ inOrder dr

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod val st dr) = postOrder st ++ postOrder dr ++ [val]

{-
*Main> b
Nod 5 (Nod 2 (Nod 1 Frunza Frunza) (Nod 4 (Nod 3 Frunza Frunza) Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 9 (Nod 8 Frunza Frunza) Frunza))
*Main> preOrder b
[5,2,1,4,3,7,6,9,8]
*Main> inOrder b
[1,2,3,4,5,6,7,8,9]
*Main> postOrder b
[1,3,4,2,6,8,9,7,5]
-}

--Ex. 4.1

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult x y = mult' x y Zero Zero

mult' :: Nat -> Nat -> Nat -> Nat -> Nat
mult' Zero y a b = Zero
mult' x Zero a b = Zero
mult' x y a b
  | a == x = b
  | otherwise = mult' x y (Succ a) (add b y)

exp :: Nat -> Nat -> Nat
exp x y = exp' x y Zero (Succ Zero)

exp' :: Nat -> Nat -> Nat -> Nat -> Nat
exp' Zero y a b = Zero
exp' x Zero a b = Succ Zero
exp' x y a b
  | a == y = b
  | otherwise = exp' x y (Succ a) (mult b y)

comp :: Nat -> Nat -> Bool
comp x y = comp' x y Zero

comp' :: Nat -> Nat -> Nat -> Bool
comp' x y z
  | y == z = False
  | x == z = True
  | otherwise = comp' x y (Succ z)

convert :: Nat -> Int
convert x = convertp x Zero

convertp :: Nat -> Nat -> Int
convertp x a
  | comp a x = 1 + convertp x (Succ a)
  | otherwise = 0

convert' :: Int -> Nat
convert' x
  | x == 0 = Zero
  | x > 0 = Succ (convert' (x - 1))
  | otherwise = error "Number needs to be natural"

--Ex. 6.1
data MyList = Void | Item Integer MyList deriving (Show, Eq)

--Ex. 6.2
findInt :: MyList -> Integer -> Bool
findInt Void _ = False
findInt (Item hd tl) key
  | hd == key = True
  | otherwise = findInt tl key

--Ex. 6.3

addIntEnd :: MyList -> Integer -> MyList
addIntEnd Void key = Item key Void
addIntEnd (Item hd tail) key = Item hd (addIntEnd tail key)
