import Data.List (minimumBy, partition)

--utils
listaAux :: Integer -> [Integer]
listaAux x = x : listaAux (x + 1)

lista :: [Integer]
lista = listaAux 0

--Ex. 1
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (hd : tail) = qsort left ++ [hd] ++ qsort right
  where
    (left, right) = partition (< hd) tail

myMin :: (Ord a) => [a] -> ([a] -> [a]) -> a
myMin [] _ = error "No min from empty list"
myMin list sort = head (sort list)

--Ex. 2

--implementare lui minimum din Haskell se executa in timp liniar, indiferent de input
--quicksort in cel mai rau caz, in care lista este sortata crescator sau descrescator sau
--contine acelasi numar repetat de mai multe ori are complexitatea de O(n^2), iar pentru cazul mediu
-- are complexitatea O(n*logn)

--putem observa ca sunt instante pentru minimum care se executa mai repede decit orice instanta
--care foloseste quicksort
{-
*Main> minimum (take 5000000 lista)
0
(1.51 secs, 646,380,720 bytes)
*Main> minimum (take 5000000 lista)
0
(2.08 secs, 646,380,720 bytes)
*Main> minimum (take 5000000 lista)
0
(1.19 secs, 646,380,720 bytes)
*Main> minimum (take 5000000 lista)
0
(2.06 secs, 646,380,720 bytes)
-}
{-
*Main> myMin (take 5000000 lista) qsort
0
(1.64 secs, 280,051,048 bytes)
*Main> myMin (take 5000000 lista) qsort
0
(1.71 secs, 280,051,048 bytes)
*Main> myMin (take 5000000 lista) qsort
0
(1.69 secs, 280,051,048 bytes)
*Main> myMin (take 5000000 lista) qsort
0
(1.62 secs, 280,051,048 bytes)
-}

--Ex. 3
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x : xs) = insert (insertSort xs)
  where
    insert [] = [x]
    insert (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insert ys

--pentru insert sort cel mai rau caz e o lista reverse sorted, O(n^2), O(n^2) average si O(n) in cazul in
--care lista e sortata, cazul cel mai bun
--in Haskell totusi, rezultatul minimului definit de noi nu se apropie de functia minimum
--iar cazul best case si worst-case par sa aiba rezultate similare
{-
*Main System.Random> minimum (take 500000 lista)
0
(0.15 secs, 64,675,104 bytes)
*Main System.Random> minimum (take 500000 lista)
0
(0.14 secs, 64,675,104 bytes)

*Main System.Random> myMin (take 500000 lista) insertSort
0
(0.95 secs, 238,103,064 bytes)
*Main System.Random> myMin (take 500000 lista) insertSort
0
(1.07 secs, 238,103,064 bytes)
*Main System.Random> myMin (take 500000 lista) insertSort
0
(0.95 secs, 238,103,064 bytes)

*Main System.Random> myMin (reverse (take 500000 lista)) insertSort
0
(1.25 secs, 250,103,096 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) insertSort
0
(0.76 secs, 250,103,096 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) insertSort
0
(0.75 secs, 250,103,096 bytes)
-}

--pentru selection sort, pentru fiecare numar pe care vrem sa il sortam trebuie sa il gasim si sa il stergem din
--list, deci e  (O(n) + O(n)) + (O(n-1) + O(n-1)) +... = O(n^2), care e valabil pentru orice lista
selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort [x] = [x]
selectSort list = smallest : selectSort (removeFromList smallest list)
  where
    smallest = minimum list

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (hd : tail)
  | hd == x = tail
  | otherwise = hd : removeFromList x tail

--surpinzator, acesta are cele mai apropiate rezultate de minimum definit in haskell
{-
*Main System.Random> myMin (reverse (take 500000 lista)) selectSort
0
(0.13 secs, 76,675,600 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) selectSort
0
(0.14 secs, 76,675,600 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) selectSort
0
(0.15 secs, 76,675,600 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) selectSort
0
-}

--pentru mergeSort complexitatea este mereu O(n log n), dar in implementarea mea din Haskell este
--unul din algoritmii cu cele mai proaste rezultate, probabil deoarece este nevoie de timp pentru a
--calcula lungimea si datorita functiilor take si drop
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = mergeLists (mergeSort left) (mergeSort right)
  where
    left = take (div (length list) 2) list
    right = drop (div (length list) 2) list

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists [] list = list
mergeLists list [] = list
mergeLists (hd1 : tail1) (hd2 : tail2)
  | hd1 < hd2 = hd1 : mergeLists tail1 (hd2 : tail2)
  | otherwise = hd2 : mergeLists (hd1 : tail1) tail2

{-
*Main System.Random> myMin (reverse (take 500000 lista)) mergeSort
0
(2.21 secs, 682,841,104 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) mergeSort
0
(2.90 secs, 682,841,104 bytes)
*Main System.Random> myMin (reverse (take 500000 lista)) mergeSort
0
(2.30 secs, 682,841,104 bytes)
-}

--Ex. 4
myMax :: (Ord a) => [a] -> ([a] -> [a]) -> a
myMax [] _ = error "No max from empty list"
myMax list sort = last (sort list)

--in acest caz observam diferenta mari intre algoritmi
--probabil deoarece din cauza lazy evaluation in cazurile precedent sortarile se opreau cind obtineam minimul
--si explica si motivul pentru care selection sort era cel mai bun algoritm la etapa precedenta, minimul il obtineam imdediat
--sotarile le aplicam pw worst case pentru fiecare sortare

{-
*Main System.Random> maximum (take 50000 lista)
49999
(0.01 secs, 6,502,880 bytes)

*Main System.Random> myMax (take 5000 lista) qsort
4999
(8.59 secs, 1,602,664,088 bytes)

*Main> myMax (reverse (take 5000 lista)) insertSort
4999
(10.77 secs, 3,058,865,328 bytes)

*Main> myMax (take 50000 lista) selectSort
49999
(179.80 secs, 90,768,846,680 bytes)

*Main> myMax (reverse (take 500000 lista)) mergeSort
499999
(13.84 secs, 2,163,535,552 bytes)
-}

--Ex. 5

fibAux :: [Integer] -> [Integer]
fibAux [n_2, n_1] = n : fibAux [n_1, n]
  where
    n = n_2 + n_1
fibAux _ = error "need n-2 and n-1"

fibInf :: [Integer]
fibInf = [0, 1] ++ fibAux [0, 1]

--Ex. 6

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True
hasDivisors n a b = hasDivisors n (a + 1) b

isPrime :: Integer -> Bool
isPrime n = (n > 1) && not (hasDivisors n 2 (div n 2))

trueIfPrimeInf :: [Bool]
trueIfPrimeInf = map isPrime lista

{-
*Main> take 10 trueIfPrimeInf
[False,False,True,True,False,True,False,True,False,False]
*Main> take 10 lista
[0,1,2,3,4,5,6,7,8,9]
-}

--Ex. 7
primesInf :: [Integer]
primesInf = filter isPrime lista

--Ex. 8
----------------------------------------------------------
--1

data List a = Void | E a (List a) deriving (Show, Eq)

concatList :: List a -> List a -> List a
concatList Void Void = Void
concatList l Void = l
concatList Void l = l
concatList l (E hd tail) = concatList (addToEnd l hd) tail

addToEnd :: List a -> a -> List a
addToEnd Void x = E x Void
addToEnd (E hd tail) x = E hd (addToEnd tail x)

partitionAccumulator :: (a -> Bool) -> List a -> (List a, List a) -> (List a, List a)
partitionAccumulator f Void (a, b) = (a, b)
partitionAccumulator f (E hd tail) (a, b)
  | f hd = partitionAccumulator f tail (E hd a, b)
  | otherwise = partitionAccumulator f tail (a, E hd b)

partition' :: (a -> Bool) -> List a -> (List a, List a)
partition' f l = partitionAccumulator f l (Void, Void)

head' :: List a -> a
head' Void = error "no head for empty list"
head' (E hd tail) = hd

--quicksort
qsort' :: Ord a => List a -> List a
qsort' Void = Void
qsort' (E hd tail) = concatList (qsort' left) (concatList (E hd Void) (qsort' right))
  where
    (left, right) = partition' (< hd) tail

myMin' :: (Ord a) => List a -> (List a -> List a) -> a
myMin' Void _ = error "No min from empty list"
myMin' list sort = head' (sort list)

-----------------------------------------------------
--3
--inserction sort
insertSort' :: Ord a => List a -> List a
insertSort' Void = Void
insertSort' (E hd Void) = E hd Void
insertSort' (E hd tail) = insert' (insertSort' tail)
  where
    insert' Void = E hd Void
    insert' (E hd2 tail2)
      | hd <= hd2 = E hd (E hd2 tail2)
      | otherwise = E hd2 (insert' tail2)

--selection sort
selectSort' :: Ord a => List a -> List a
selectSort' Void = Void
selectSort' (E hd Void) = E hd Void
selectSort' list = E smallest (selectSort' (removeFromList' smallest list))
  where
    smallest = minimum' list

minimum' :: Ord a => List a -> a
minimum' Void = error "cant find void in empty list"
minimum' (E hd Void) = hd
minimum' (E hd tail)
  | hd < partialMin = hd
  | otherwise = partialMin
  where
    partialMin = minimum' tail

removeFromList' :: Eq a => a -> List a -> List a
removeFromList' _ Void = Void
removeFromList' x (E hd tail)
  | hd == x = tail
  | otherwise = E hd (removeFromList' x tail)

--merge sort
mergeSort' :: Ord a => List a -> List a
mergeSort' Void = Void
mergeSort' (E hd Void) = E hd Void
mergeSort' list = mergeLists' (mergeSort' left) (mergeSort' right)
  where
    (left, right) = splitInTwoHalves list

mergeLists' :: Ord a => List a -> List a -> List a
mergeLists' Void Void = Void
mergeLists' Void list = list
mergeLists' list Void = list
mergeLists' (E hd1 tail1) (E hd2 tail2)
  | hd1 < hd2 = E hd1 (mergeLists' tail1 (E hd2 tail2))
  | otherwise = E hd2 (mergeLists' (E hd1 tail1) tail2)

length' :: List a -> Integer
length' Void = 0
length' (E hd tail) = 1 + length' tail

splitInTwoHalves :: List a -> (List a, List a)
splitInTwoHalves list = splitInTwo list 0 (div (length' list) 2) Void Void

splitInTwo :: List a -> Integer -> Integer -> List a -> List a -> (List a, List a)
splitInTwo Void _ _ left right = (left, right)
splitInTwo (E hd tail) cumulator limit left right
  | cumulator < limit = splitInTwo tail (cumulator + 1) limit (addToEnd left hd) right
  | otherwise = splitInTwo tail (cumulator + 1) limit left (addToEnd right hd)

-------------------------------------------------
--4
last' :: List a -> a
last' Void = error "can't take last element from empty list"
last' (E hd Void) = hd
last' (E hd tail) = last' tail

myMax' :: (Ord a) => List a -> (List a -> List a) -> a
myMax' Void _ = error "No max from empty list"
myMax' list sort = last' (sort list)

--------------------------------------------------
--5

fibAux' :: List Integer -> List Integer
fibAux' (E n_2 (E n_1 Void)) = E n (fibAux' (E n_1 (E n Void)))
  where
    n = n_2 + n_1
fibAux' _ = error "need n-2 and n-1"

fibInf' :: List Integer
fibInf' = concatList (E 0 (E 1 Void)) (fibAux' (E 0 (E 1 Void)))

-------------------------------------------------------
--6
--utils for generating infinite natural numbers
naturalAux :: Integer -> List Integer
naturalAux x = E x (naturalAux (x + 1))

natural :: List Integer
natural = naturalAux 0

--true if number is prime

trueIfPrimeInf' :: List Bool
trueIfPrimeInf' = map' isPrime natural

map' :: (a -> b) -> List a -> List b
map' _ Void = Void
map' f (E hd tail) = E (f hd) (map' f tail)

------------------------------------------------
--7
primesInf' :: List Integer
primesInf' = filter' isPrime natural

filter' :: (a -> Bool) -> List a -> List a
filter' f Void = Void
filter' f (E hd tail)
  | f hd = E hd (filter' f tail)
  | otherwise = filter' f tail
