{-
    Ex. 2.1

    Prelude> 2
    2
    Prelude> 2 + 3
    5
    Prelude> 2 + 3 * 5
    17
    Prelude> (2 + 3) *5
    25
    Prelude> 3/5
    0.6
    Prelude> 134132413134 * 1341324343
    179915070921967120962
    Prelude> 3/0
    Infinity
    Prelude> True
    True
    Prelude> False
    False
    Prelude> True && False
    False
    Prelude> True || False
    True
    Prelude> not True
    False
    Prelude> 2 <= 3
    True
    Prelude> not (2 <= 3)
    False
    Prelude> (2 <= 3) || True
    True
    Prelude> "aaa" == "aba"
    False
    Prelude> "aba" == "aba"
    True
    Prelude> "aaa" ++ "aba"
    "aaaaba"
-}
{-
    Ex. 2.2

    Prelude> 2
    2
    Prelude> (+) 2 3
    5
    Prelude> (+) 2 ((*) 3 5)
    17
    Prelude> (*) ((+) 2 3) 5
    25
    Prelude> (/) 3 5
    0.6
    Prelude>  (*) 45345345346536 54425523454534333
    2467944156711854340070394620488
    Prelude> (/) 3 0
    Infinity
    Prelude> True
    True
    Prelude> False
    False
    Prelude> (&&) True False
    False
    Prelude> (||) True False
    True
    Prelude> not True
    False
    Prelude> (<=) 2 3
    True
    Prelude> not ((<=) 2 3)
    False
    Prelude> (||) ((<=) 2 3) True
    True
    Prelude> (==) "aaa" "aba"
    False
    Prelude> (==) "aba" "aba"
    True
    Prelude> (++) "aaa" "aba"
    "aaaaba"
-}

{-
    Ex. 2.3

    Prelude> :t True
    True :: Bool
    Prelude> :t False
    False :: Bool
    Prelude> :t True && False
    True && False :: Bool
    Prelude> :t True && (2 <= 4)
    True && (2 <= 4) :: Bool
-}

{-
    Ex. 2.4

    Prelude> :t "aaa"
    "aaa" :: [Char]
-}

{-
    Ex. 2.5

    Prelude> :t 2
    2 :: Num p => p
    Prelude> :t 2 + 3
    2 + 3 :: Num a => a
    Prelude> :t (+)
    (+) :: Num a => a -> a -> a
-}

{-
    Ex. 2.6

    Prelude> not 2

    <interactive>:9:5: error:
        * No instance for (Num Bool) arising from the literal `2'
        * In the first argument of `not', namely `2'
        In the expression: not 2
        In an equation for `it': it = not 2
-}

{-
    Ex. 2.7

    Prelude> :t not
    not :: Bool -> Bool
    Prelude> :t 2
    2 :: Num p => p

    Observam ca functia not ia ca input un argument Bool, dar 2 este de tip p care = Num
-}

{-
    Ex. 3.1

    Prelude> :t succ
    succ :: Enum a => a -> a
    Prelude> succ 1
    2
    Prelude> :t pred
    pred :: Enum a => a -> a
    Prelude> pred 1
    0
    Prelude> :t max
    max :: Ord a => a -> a -> a
    Prelude> max (-1) 1
    1
    Prelude> :t min
    min :: Ord a => a -> a -> a
    Prelude> min (-1) 1
    -1
-}

{-
    Ex. 3.2

    Prelude> let id x = x
    Prelude> id 3
    3
-}

{-
    Ex. 3.3

    Prelude> let sumThree x y z = x + y + z
    Prelude> sumThree 1 2 3
    6
-}

{-
    Ex. 3.4

    Prelude> let prodOf3 x y z = x * y * z
    Prelude> prodOf3 1 2 3
    6
-}

{-
    Ex. 3.5

    PS C:\Users\roboc> ghci
    GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
    Prelude> :load labPF/functii.hs
    [1 of 1] Compiling Main             ( labPF\functii.hs, interpreted )
    Ok, one module loaded.
    *Main>
-}

{-
    Ex. 3.6

    *Main> id 2

    <interactive>:2:1: error:
        Ambiguous occurrence `id'
        It could refer to
        either `Prelude.id',
                imported from `Prelude' at labPF\functii.hs:1:1
                (and originally defined in `GHC.Base')
            or `Main.id', defined at labPF\functii.hs:1:1
    *Main> Main.id 2
    2
    *Main> sumThree 1 2 3
    6
-}

{-
    Ex. 3.7

    *Main> :t sumThree
    sumThree :: Num a => a -> a -> a -> a
    *Main> sumThree 3.2 2 4
    9.2
-}

{-
    Ex. 3.8

    *Main> :r
    [1 of 1] Compiling Main             ( labPF\functii.hs, interpreted )
    Ok, one module loaded.
    *Main> :t sumThree
    sumThree :: Int -> Int -> Int -> Int
    *Main> sumThree 3.2 2 4

    <interactive>:9:10: error:
        * No instance for (Fractional Int) arising from the literal `3.2'
        * In the first argument of `sumThree', namely `3.2'
        In the expression: sumThree 3.2 2 4
        In an equation for `it': it = sumThree 3.2 2 4
-}

{-
    Ex. 3.9

    *Main> :t myMax
    myMax :: Int -> Int -> Int
    *Main> myMax 1 3
    3
-}

--Ex. 3.10
maxThree x y z = if x > y then (if x > z then x else (if y > z then y else z)) else (if y > z then y else z)

{-
    *Main> maxThree 1 2 3
    3
-}

--Ex. 3.11
mySum :: Int -> Int
mySum x = if x <= 0 then 0 else x + mySum (x - 1)

{-
    *Main> mySum 15
    120
-}

--Ex. 3.12
myFibonaci :: Int -> Int
myFibonaci x =
  if x <= 0
    then 0
    else
      ( if x == 1
          then 1
          else myFibonaci (x - 1) + myFibonaci (x - 2)
      )

--Ex. 3.13
myCmmdc :: Int -> Int -> Int
myCmmdc x y
  | x == 0 && y == 0 = 0
  | x == y = x
  | x > y = myCmmdc (x - y) y
  | otherwise = myCmmdc x (y - x)
