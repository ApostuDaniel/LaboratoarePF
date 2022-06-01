module Main where
import Nat
import NatConversion

main :: IO ()
main = do
        putStrLn "Input the first number"
        input1 <- getLine
        putStrLn "Input the second number"
        input2 <- getLine
        let firstNumber = read input1 :: Int
        let secondNumber = read input2 :: Int
        let naturalFirst = intToNat firstNumber
        let naturalSecond = intToNat secondNumber
        let sum = naturalFirst + naturalSecond
        putStrLn ("The result is: " ++ show (natToInt sum))

