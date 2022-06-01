module NatConversion(natToInt, intToNat) where
import Nat

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + (natToInt x)

intToNat :: Int -> Nat
intToNat x
    | x == 0 = Zero
    | x < 0 = error "Can't convert negative integer to natural number"
    | otherwise = (Succ (intToNat (x - 1)))