import Test.QuickCheck
import Nat
import NatConversion

prop_p1 :: Int -> Property
prop_p1 x = (x >= 0) ==> (natToInt $ intToNat x) == x

prop_p2 :: Nat -> Bool
prop_p2 x = (intToNat $ natToInt x) == x



main :: IO ()
main = do 
    putStrLn "Tests start"
    quickCheck prop_p1
    quickCheck (verbose prop_p2)
    putStrLn "Tests end"
