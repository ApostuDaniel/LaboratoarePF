module Nat where
import Test.QuickCheck
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

instance Num Nat where
  fromInteger x 
    | x == 0 = Zero
    | x < 0 = fromInteger (-x)
    | otherwise = Succ (fromInteger x - 1)
  Zero + Zero = Zero
  Zero + (Succ x) = Succ x
  (Succ x) + Zero = Succ x
  (Succ x) + (Succ y) = Succ (x + Succ y)
  Zero * Zero = Zero
  Zero * Succ x = Zero
  Succ x * Zero = Zero
  (Succ x) * (Succ y)
    | x == Zero = y
    | y == Zero = x
    | otherwise = Succ x + (Succ x) * y
  Zero - Zero = Zero
  Zero - (Succ x) = Zero
  (Succ x) - Zero = Succ x
  (Succ x) - (Succ y) = x - y
  abs x = x
  signum Zero = Zero
  signum (Succ x) = Succ Zero

instance Arbitrary Nat where
--  arbitrary = fromInteger <$> arbitrary
  arbitrary = oneof [return Zero, return (Succ Zero), return (Succ (Succ Zero))]