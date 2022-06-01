import GHC.StgToCmm.ArgRep (ArgRep (V))

type Id = String

data Term
  = Var Id
  | App Term Term
  | Lambda Id Term
  deriving (Show, Eq)

--Ex.0.1
x = Var "x"

lxlyx :: Term
lxlyx = Lambda "x" (Lambda "y" x)

--Ex.0.2

subst :: Id -> Term -> Term -> Term
subst id term (Var id')
  | id == id' = term
  | True = (Var id')
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term')
  | id == id' = Lambda id' term'
  | True = (Lambda id' (subst id term term'))

y = Var "y"

z = Var "z"

{-
*Main> subst "x" (Var "y") (Var "x")
Var "y"

*Main> subst "y" (Var "z") (Var "x")
Var "x"

*Main> subst "y" (Var "z") (App (Var "x") (Var "y"))
App (Var "x") (Var "z")

*Main> subst "y" z (App y x)
App (Var "z") (Var "x")

*Main> subst "x" (Lambda "z" z) (Lambda "x"( App y x))
Lambda "x" (App (Var "y") (Var "x"))

*Main> subst "x" (Lambda "z" z) (Lambda "y"( App y x))
Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

*Main> subst "y" x (Lambda "x"(Var "y"))
Lambda "x" (Var "x")
-}
--Ex.0.3
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd : tl)
  | id == hd = remove id tl
  | True = hd : remove id tl

{-
*Main> remove "x" ["x"]
[]
*Main> remove "x" ["x", "z"]
["z"]
*Main> remove "x" ["y", "z", "y", "k"]
["y","z","y","k"]
*Main> remove "x" ["y", "x", "x", "k"]
["y","k"]
-}

--Ex.0.4
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2)
free (Lambda id term) = remove id (free term)

--Ex.0.5
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2)
vars (Lambda id term) = vars term

--Ex.0.6
fresh' :: [Id] -> Int -> Id
fresh' ids index =
  if ("n" ++ (show index)) `elem` ids
    then fresh' ids (index + 1)
    else "n" ++ (show index)

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _
  | id == id' = term
  | True = (Var id')
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
  | id == id' = Lambda id' term'
  | id' `elem` (free term) =
    let id'' = fresh avoid
     in Lambda id'' (casubst id term (subst id' (Var id'') term') (id'' : avoid))
  | True = Lambda id' (casubst id term term' avoid)

{-
*Main> casubst "x" y x []
Var "y"

*Main> casubst "y" z x []
Var "x"

*Main> casubst "y" z  (App x y) []
App (Var "x") (Var "z")

*Main> casubst "y" z  (App y x) []
App (Var "z") (Var "x")

*Main> casubst "x" (Lambda "z" z) (Lambda "x" (App y x)) []
Lambda "x" (App (Var "y") (Var "x"))

*Main> casubst "x" (Lambda "z" z) (Lambda "y" (App y x)) []
Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

*Main> casubst "y" x (Lambda "x" y) []
Lambda "n0" (Var "x")
-}

--Ex.0.8
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid) --beta-reducerea propriu-zisa
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
  Nothing -> case reduce1' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
  Nothing -> Nothing
  Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

term1 = Lambda "x" x

term2 = App term1 term1

term3 = Lambda "y" (Lambda "x" term2)

term4 = App term3 term1

ex1 = reduce1 term1 -- Nothing

ex2 = reduce1 term2 -- Just (\x.x)

ex3 = reduce1 term3 -- Just \y.\x.(\x.x)

ex4 = reduce1 term4 -- Just \x.(\x.x \x.x)

--Ex.0.9
reduce :: Term -> Term
reduce term = case reduce1 term of
  Nothing -> term
  Just term' -> reduce term'

term5 = Lambda "z" z

term6 = App term1 (App term5 y)

exReduced = reduce term6 -- Var "y"
{-
*Main> return term6 >>= reduce1
Just (App (Lambda "z" (Var "z")) (Var "y"))

*Main> return term6 >>= reduce1 >>= reduce1
Just (Var "y")
-}

--Ex.0.10
infinitePart = App (Lambda "x" x) x

infinite = App infinitePart infinitePart

reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of
  Nothing -> term
  Just term' -> reduceFor (n -1) term'

--Ex.0.11
tTRUE = Lambda "x" (Lambda "y" x)

tFALSE = Lambda "x" (Lambda "y" y)

u = Var "u"

v = Var "v"

tAND = Lambda "u" (Lambda "v" (App (App u v) u))

tOR = Lambda "u" (Lambda "v" (App (App u u) v))

tNOT = Lambda "u" (App (App u tFALSE) tTRUE)

{-
*Main> reduce (App (App tAND tTRUE) tFALSE)
Lambda "x" (Lambda "y" (Var "y"))

*Main> reduce (App (App tOR tTRUE) tFALSE)
Lambda "x" (Lambda "y" (Var "x"))

*Main> reduce (App tNOT tTRUE)
Lambda "x" (Lambda "y" (Var "y"))
-}

--Ex.0.12
n = Var "n"

f = Var "f"

m = Var "f"

zero = Lambda "f" (Lambda "x" x)

lsucc = Lambda "n" (Lambda "f" (Lambda "x" (App (App n f) (App f x))))

one = reduce (App lsucc zero) --Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

two = reduce (App lsucc one)

--incorect
tPLUS = App (Lambda "n" (Lambda "m" (Lambda "f" (Lambda "x" (App m f))))) (App n (App f x))

plus' :: Term -> Term -> Term
plus' term1 term2 = reduce (App (App tPLUS term1) term2)

three = plus' one two