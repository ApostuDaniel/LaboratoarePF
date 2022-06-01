type Id = String

data Term
  = Var Id
  | App Term Term
  | Lambda Id Term
  deriving (Show, Eq)

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2)
vars (Lambda id term) = vars term

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2)
free (Lambda id term) = remove id (free term)

fresh' :: [Id] -> Int -> Id
fresh' ids index =
  if ("n" ++ (show index)) `elem` ids
    then fresh' ids (index + 1)
    else "n" ++ (show index)

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd : tl)
  | id == hd = remove id tl
  | True = hd : remove id tl

subst :: Id -> Term -> Term -> Term
subst id term (Var id')
  | id == id' = term
  | True = (Var id')
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term')
  | id == id' = Lambda id' term'
  | True = (Lambda id' (subst id term term'))

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

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid)
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

--Intrebare
z = Var "z"

x1 = Var "x1"

x2 = Var "x2"

x3 = Var "x3"

y = Var "y"

x = Var "x"

expr1 = App (Lambda "x1" x1) (App (Lambda "x2" x2) (Lambda "z" (App (Lambda "x3" x3) z)))

expr2 = App (Lambda "x1" (Lambda "x2" x2)) (App (Lambda "x" x) (Lambda "y" y))

--Strategia de evaluare implementata este Normal Order
red1 = reduce1 expr1 -- Just (App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))))

red2 = reduce1 expr2 -- Just (Lambda "x2" (Var "x2"))

--Ex.0.2
cbn' :: Term -> [Id] -> Maybe Term
cbn' (Var id') _ = Nothing
cbn' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid)
cbn' (App term1 term2) avoid = case cbn' term1 avoid of
  Nothing -> case cbn' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
cbn' (Lambda id (App (Lambda x y) z)) avoid = Nothing
cbn' (Lambda id term) avoid = Nothing

cbn :: Term -> Maybe Term
cbn t = cbn' t (vars t)

{-
*Main> cbn expr1 >>= cbn
Just (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z")))

*Main> cbn expr1 >>= cbn >>= cbn
Nothing

*Main> cbn expr2
Just (Lambda "x2" (Var "x2"))
-}
--Un avantaj putem observa in reducerea expr2, unde datorita faptului ca
-- x1 nu mai este folosit in calcul, nu mai trebuie sa calculam valoare lui App (Lambda "x" x) (Lambda "y" y)

expr3 = App (Lambda "x1" (App x1 x1)) (App (Lambda "x" x) (Lambda "y" y))

{-
*Main> cbn expr3
Just (App (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))

*Main> cbn expr3 >>= cbn
Just (App (Lambda "y" (Var "y")) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

*Main> cbn expr3 >>= cbn >>= cbn
Just (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

*Main> cbn expr3 >>= cbn >>= cbn >>= cbn
Just (Lambda "y" (Var "y"))
-}

--Ex.0.3

strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid =
  [casubst id term' term avoid]
    ++ let all = strategy1' term avoid
        in let all' = strategy1' term' avoid
            in [App (Lambda id successorTerm) term' | successorTerm <- all]
                 ++ [App (Lambda id term) successorTerm' | successorTerm' <- all']
strategy1' (App term1 term2) avoid =
  let all1 = strategy1' term1 avoid
   in let all2 = strategy1' term2 avoid
       in [App sterm1 term2 | sterm1 <- all1]
            ++ [App term1 sterm2 | sterm2 <- all2]
strategy1' (Lambda id term) avoid =
  let all = strategy1' term avoid
   in [Lambda id sterm | sterm <- all]

strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)

strategy :: Term -> [Term]
strategy term =
  let all = strategy1 term
   in case all of
        [] -> [term]
        _ -> concat (map strategy all)

--Aceasta strategie este Strategia Full Beta-Reduction
{-
*Main> strategy1 expr1
[App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))),App (Lambda "x1" (Var "x1")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))),App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (Lambda "z" (Var "z")))]

*Main> strategy1 expr2
[Lambda "x2" (Var "x2"),App (Lambda "x1" (Lambda "x2" (Var "x2"))) (Lambda "y" (Var "y"))]

*Main> strategy1 expr3
[App (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))),App (Lambda "x1"
(App (Var "x1") (Var "x1"))) (Lambda "y" (Var "y"))]
-}

--Ex.0.4
cbv' :: Term -> [Id] -> Maybe Term
cbv' (Var id') _ = Nothing
cbv' (App (Lambda id term) term') avoid = case cbv' term' avoid of
  Nothing -> Just (casubst id term' term avoid)
  Just term1' -> Just (App (Lambda id term) term1')
cbv' (App term1 term2) avoid = case cbv' term1 avoid of
  Nothing -> case cbv' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
cbv' (Lambda id (App (Lambda x y) z)) avoid = Nothing
cbv' (Lambda id term) avoid = Nothing

cbv :: Term -> Maybe Term
cbv t = cbv' t (vars t)

{-
*Main> expr1
App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))))

*Main> cbv expr1
Just (App (Lambda "x1" (Var "x1")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))))
*Main> cbv expr1 >>= cbv
Just (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z")))
*Main> cbv expr1 >>= cbv >>= cbv
Nothing
-}

--Ex.0.5

p1 = expr2

p2 = expr3

{-
Main> p1
App (Lambda "x1" (Lambda "x2" (Var "x2"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

*Main> cbv p1
Just (App (Lambda "x1" (Lambda "x2" (Var "x2"))) (Lambda "y" (Var "y")))

*Main> cbv p1 >>= cbv
Just (Lambda "x2" (Var "x2"))

*Main> cbn p1
Just (Lambda "x2" (Var "x2"))

For the first expresion Call by Name is more effiecient since we avoid one reduction step
-----------------------------------------------
*Main> p2
App (Lambda "x1" (App (Var "x1") (Var "x1"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

*Main> cbv p2
Just (App (Lambda "x1" (App (Var "x1") (Var "x1"))) (Lambda "y" (Var "y")))

*Main> cbv p2 >>= cbv
Just (App (Lambda "y" (Var "y")) (Lambda "y" (Var "y")))

*Main> cbv p2 >>= cbv >>= cbv
Just (Lambda "y" (Var "y"))

*Main> cbn p2
Just (App (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))

*Main> cbn p2 >>= cbn
Just (App (Lambda "y" (Var "y")) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))

*Main> cbn p2 >>= cbn >>= cbn
Just (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

*Main> cbn p2 >>= cbn >>= cbn >>= cbn
Just (Lambda "y" (Var "y"))

For the second expression Call by Values is more eficient since we avoid reducing App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")) 2 times.
-}

--Ex.0.6
aplicative' :: Term -> [Id] -> Maybe Term
aplicative' (Var id') _ = Nothing
aplicative' (App (Lambda id term) term') avoid = case aplicative' term' avoid of
  Nothing -> Just (casubst id term' term avoid)
  Just term1' -> Just (App (Lambda id term) term1')
aplicative' (App term1 term2) avoid = case aplicative' term1 avoid of
  Nothing -> case aplicative' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
aplicative' (Lambda id term) avoid = case aplicative' term avoid of
  Nothing -> Nothing
  Just term' -> Just (Lambda id term')

aplicative :: Term -> Maybe Term
aplicative t = reduce1' t (vars t)