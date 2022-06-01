import Control.Exception (handle)
import Data.Char (toUpper)
import GHC.IO.IOMode (IOMode (ReadMode))
import System.Environment
import System.IO

--Ex. 0.1
--Singura valoare de tip () este ()

--Ex. 0.2
{-
main :: IO ()
main = putStrLn "Hello, World!" >> putStrLn "Ciao Mondo" >> putStrLn "Privet Mir"
-}
--(>>) este asociativ implicit la stinga
--infixl 1 >>

--Ex. 0.3
{-
main :: IO ()
main =
  putStrLn "What is your first name?"
    >> ( getLine
           >>= \firstName ->
             putStrLn "What is you last name"
               >> getLine
               >>= \lastName -> putStrLn ("Hello " ++ firstName ++ " " ++ lastName)
       )-}

--Ex. 0.4
{-
main :: IO ()
main = do
  putStrLn "What is your first name?"
  firstName <- getLine
  putStrLn "What is you last name"
  lastName <- getLine
  putStrLn ("Hello " ++ firstName ++ " " ++ lastName)
-}

--Ex. 0.5
{-
main :: IO ()
main =
  putStrLn "What is your name?"
    >> ( getLine
           >>= \name -> putStrLn ("Hello, " ++ name ++ "!") >> main
       )
-}

--Ex. 0.6
{-
main :: IO ()
main =
  putStrLn "What is your first name?"
    >> ( getLine
           >>= \firstName ->
             putStrLn "What is you last name"
               >> getLine
               >>= \lastName ->
                 putStrLn ("Hello " ++ firstName ++ " " ++ lastName)
                   >> main
       )
-}

--Ex. 0.7
{-
main :: IO ()
main =
  putStrLn "What is your first name?"
    >> ( getLine
           >>= \firstName ->
             if firstName == ""
               then return ()
               else
                 putStrLn "What is you last name"
                   >> getLine
                   >>= \lastName ->
                     if lastName == ""
                       then return ()
                       else
                         putStrLn ("Hello " ++ firstName ++ " " ++ lastName)
                           >> main
       )
-}

--Ex. 0.8
{-
main :: IO ()
main = do
  putStrLn "Input string"
  firstName <- getLine
  if firstName == ""
    then return ()
    else putStrLn (map toUpper firstName) >> main
-}

--Ex. 0.9
--System.IO
-- openFile :: FilePath -> IOMode -> IO Handle
-- hGetContents :: Handle -> IO String
-- hGetLine :: Handle -> IO String
-- hClose :: Handle -> IO ()
--System.Environment
-- getArgs :: IO [String]  -- Defined in `System.Environment'
-- getProgName :: IO String
-- hPutStr :: Handle -> String -> IO ()

--Ex. 0.10
{-
main :: IO ()
main = do
  handle <- openFile "exemplu.txt" ReadMode
  continut <- hGetContents handle
  putStrLn continut-}

--Ex. 0.11
{-
main :: IO ()
main = do
  (fisier : _) <- getArgs
  handle <- openFile fisier ReadMode
  continut <- hGetContents handle
  putStrLn continut
-}

--Ex. 0.12
{-
main :: IO ()
main = do
  (fisier : rest) <- getArgs
  if fisier == "" || null fisier || not (null rest)
    then do
      nameOfProgram <- getProgName
      putStrLn (nameOfProgram ++ " file ")
    else do
      handle <- openFile fisier ReadMode
      continut <- hGetContents handle
      putStrLn continut
-}

--Ex. 0.13
{-
main :: IO ()
main = do
  (fisier : rest) <- getArgs
  if fisier == "" || null fisier || not (null rest)
    then do
      nameOfProgram <- getProgName
      putStrLn (nameOfProgram ++ " file ")
    else do
      handle <- openFile fisier ReadMode
      continut <- hGetContents handle
      putStrLn (map toUpper continut)-}

--Ex. 0.13
{-
main :: IO ()
main = do
  (fisier : rest) <- getArgs
  if fisier == "" || null fisier || not (null rest)
    then do
      nameOfProgram <- getProgName
      putStrLn (nameOfProgram ++ " file ")
    else do
      handle <- openFile fisier ReadMode
      continut <- hGetContents handle
      putStrLn (map toUpper continut)
-}

mid :: Int -> Int -> Int
mid start end = start + div (end - start) 2

--Ex. 0.14
binarySearch :: Int -> Int -> Int -> IO ()
binarySearch x start end = do
  putStrLn ("Numarul de ghicit este >= " ++ show middle ++ " ?" ++ " numerele sunt " ++ show start ++ " " ++ show end)
  raspuns <- getLine
  if start == end - 1
    then
      if raspuns == "Da"
        then putStrLn ("Atunci numarul este " ++ show start)
        else putStrLn ("Atunci numarul este " ++ show end)
    else
      if raspuns == "Da"
        then binarySearch x (middle + 1) end
        else
          if raspuns == "Nu"
            then binarySearch x start (middle - 1)
            else putStrLn "Nu am inteles. " >> binarySearch x start end
  where
    middle = mid start end

main = do
  (input : rest) <- getArgs
  let numar = read input :: Int
  if numar < 1 || numar > 100
    then putStrLn "numarul nu este intre 1 si 100"
    else binarySearch numar 1 100

--Ex. 0.15
--Ne ajuta sa procesam treptat fisiere foarte mari fara sa le incarcam complet in memorie, putem lucra doar cu partea de la inceput de care
--avem nevoie