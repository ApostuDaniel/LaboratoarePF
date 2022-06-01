import Prelude hiding (and, div, mod, not, or, succ)

--Exercitiul 1

and :: Bool -> Bool -> Bool
and False _ = False
and _ False = False
and _ _ = True

{-
*Main> and True False
False
*Main> and False True
False
*Main> and True True
True
*Main> and False False
False
-}

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

{-
*Main> or True False
True
*Main> or False True
True
*Main> or True True
True
*Main> or False False
False
-}
not :: Bool -> Bool
not False = True
not True = False

{-
*Main> not True
False
*Main> not False
True
-}

nand :: Bool -> Bool -> Bool
nand x y = not (and x y)

{-
*Main> nand False False
True
*Main> nand False True
True
*Main> nand True False
True
*Main> nand True True
False
-}

nor :: Bool -> Bool -> Bool
nor x y = not (or x y)

{-
*Main> nor False False
True
*Main> nor False True
False
*Main> nor True False
False
*Main> nor True True
False
-}

imp :: Bool -> Bool -> Bool
imp True False = False
imp _ _ = True

{-
*Main> imp False False
True
*Main> imp False True
True
*Main> imp True False
False
*Main> imp True True
True
-}

doubleImp :: Bool -> Bool -> Bool
doubleImp True True = True
doubleImp False False = True
doubleImp _ _ = False

{-
*Main> doubleImp False False
True
*Main> doubleImp False True
False
*Main> doubleImp True False
False
*Main> doubleImp True True
True
-}
-------------------------------------------

--Exercitiul 2

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True
hasDivisors n a b = hasDivisors n (a + 1) b

{-
*Main> hasDivisors 15 3 5
True
*Main> hasDivisors 15 4 5
True
*Main> hasDivisors 15 6 7
False
-}

isPrime :: Integer -> Bool
isPrime n = and (n > 1) (not (hasDivisors n 2 (div n 2)))

{-
*Main> isPrime 0
False
*Main> isPrime 1
False
*Main> isPrime 2
True
*Main> isPrime 3
True
*Main> isPrime 4
False
*Main> isPrime 17
True
*Main> isPrime 18
False
-}
--------------------------------------------

--Exercitiul 3

cmmdcMinus :: Int -> Int -> Int
cmmdcMinus x y
  | x == 0 && y == 0 = 0
  | x == y = x
  | x > y = cmmdcMinus (x - y) y
  | otherwise = cmmdcMinus x (y - x)

{-
*Main> cmmdcMinus 6 2
2
*Main> cmmdcMinus 6 4
2
*Main> cmmdcMinus 36 18
18
*Main> cmmdcMinus 36 21
3
-}

cmmdcDiv :: Integer -> Integer -> Integer
cmmdcDiv x y
  | y == 0 = x
  | otherwise = cmmdcDiv y (mod x y)

{-
*Main> cmmdcDiv 6 2
2
*Main> cmmdcDiv 6 4
2
*Main> cmmdcDiv 36 18
18
*Main> cmmdcDiv 36 3
3
-}

cmmdcBin :: Integer -> Integer -> Integer
cmmdcBin u v
  | u == 0 = v
  | v == 0 = u
  | and (even u) (even v) = 2 * cmmdcBin (div u 2) (div v 2)
  | even u = cmmdcBin (div u 2) v
  | even v = cmmdcBin u (div v 2)
  | otherwise = cmmdcBin (abs (u - v)) (min u v)

{-
*Main> cmmdcBin 6 2
2
*Main> cmmdcBin 6 4
2
*Main> cmmdcBin 36 18
18
*Main> cmmdcBin 36 21
3
-}

--Exerctitiul 4
{-
Nu, apelurile recursive deja sunt in pozitie de coada
-}

--Exercitiul 5

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

{-
*Main> fibo 0
0
*Main> fibo 1
1
*Main> fibo 10
55
-}

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n -1) b (a + b)

-- a si b sunt doua numere Fibonacci consecutive
fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

{-
*Main> fibo' 0
0
*Main> fibo' 1
1
*Main> fibo' 10
55
-}

type Matrix = [[Integer]]

fibo'' :: Integer -> Integer
fibo'' n
  | n == 0 = 0
  | otherwise = head (head x)
  where
    x = fiboPower [[1, 1], [1, 0]] (n - 1)

fiboPower :: Matrix -> Integer -> Matrix
fiboPower m n
  | or (n == 0) (n == 1) = m
  | even n = fiboMultiply a a
  | otherwise = fiboMultiply (fiboMultiply a a) [[1, 1], [1, 0]]
  where
    a = fiboPower m (div n 2)

fiboMultiply :: Matrix -> Matrix -> Matrix
fiboMultiply m1 m2 =
  [ [ head (head m1) * head (head m2) + head (last m1) * last (head m2),
      head (head m1) * head (last m2) + head (last m1) * last (last m2)
    ],
    [ last (head m1) * head (head m2) + last (last m1) * last (head m2),
      last (head m1) * head (last m2) + last (last m1) * last (last m2)
    ]
  ]

{-
*Main> fibo'' 0
0
*Main> fibo'' 1
1
*Main> fibo'' 2
1
*Main> fibo'' 3
2
*Main> fibo'' 4
3
*Main> fibo'' 11
89
-}

-----------------------------------------------------

--Exercitiul 6

euclidExtins :: Integer -> Integer -> (Integer, Integer, Integer)
euclidExtins a b
  | a == 0 = (b, 0, 1)
  | otherwise = (gcd, y - div b a * x, x)
  where
    (gcd, x, y) = euclidExtins (mod b a) a

{-
*Main> euclidExtins 144 54
(18,-1,3)
*Main> euclidExtins 36 24
(12,1,-1)
*Main> euclidExtins 17 2
(1,1,-8)
-}
------------------------------------------------

--Exercitiul 7

succ :: Integer -> Integer
succ n = n + 1

{-
*Main> succ (-123)
-122
*Main> succ 1
2
*Main> succ 2
3
-}
----------------------------------------
--Exercitiul 8

addition :: Integer -> Integer -> Integer
addition m 0 = m
addition 0 n = n
addition m n = succ (addition m (n - 1))

{-
*Main> addition 2 3
5
*Main> addition 3 2
5
*Main> addition 3 4
7
-}

multiply :: Integer -> Integer -> Integer
multiply m n
  | or (m == 0) (n == 0) = 0
  | m == 1 = n
  | n == 1 = m
  | otherwise = addition m (multiply m (n - 1))

{-
*Main> multiply 2 0
0
*Main> multiply 2 1
2
*Main> multiply 3 7
21
-}

power :: Integer -> Integer -> Integer
power m n
  | m == 0 = 0
  | n == 0 = 1
  | m == 1 = 1
  | n == 1 = m
  | otherwise = multiply m (power m (n - 1))

{-
*Main> power 2 0
1
*Main> power 0 5
0
*Main> power 2 2
4
*Main> power 2 7
128
*Main> power 2 3
8
*Main> power 3 3
27
-}

div :: Integer -> Integer -> Integer
div a 0 = error "Division by zero is undefined"
div a b = diva a b 0

diva :: Integer -> Integer -> Integer -> Integer
diva a b c
  | a - b >= 0 = diva (a - b) b (c + 1)
  | otherwise = c

{-
*Main> div 0 2
0
*Main> div 6 2
3
*Main> div 6 5
1
*Main> div 6 7
0
-}

mod :: Integer -> Integer -> Integer
mod a b
  | b == 0 = error "Mod by zero is undefined"
  | a >= b = mod (a - b) b
  | otherwise = a

{-
*Main> mod 0 2
0
*Main> mod 2 0
*** Exception: Mod by zero is undefined
CallStack (from HasCallStack):
  error, called at labPF/Apostu_Daniel_2A6_lab2.hs:351:14 in main:Main
*Main> mod 6 2
0
*Main> mod 6 5
1
*Main> mod 6 7
6
-}
