type Id = String

data Term = Var Id
    | App Term Term
    | Lambda Id Term deriving (Show, Eq)

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

reduce :: Term -> Term
reduce term = case reduce1 term of
  Nothing -> term
  Just term' -> reduce term'

x = Var "x"
y = Var "y"
u = Var "u"
v = Var "v"
n = Var "n"
f = Var "f"
n1 = Var "n1"
n2 = Var "n2"
g = Var "g"
h = Var "h"
z = Var "z"
p = Var "p"
t = Var "t"
l = Var "l"
d = Var "d"

true' = Lambda "x" (Lambda "y" x)

false' = Lambda "x" (Lambda "y" y)

and' = Lambda "u" (Lambda "v" (App (App u v) u))

or' = Lambda "u" (Lambda "v" (App (App u u) v))

not' = Lambda "u" (App (App u false') true')

--Ex.0.1

almostFalse1 = reduce1 (App not' true')
{-
Main> almostFalse1
Just (App (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Lambda "y" (Var "y")))) (Lambda "x" (Lambda "y" (Var "x"))))
*Main> almostFalse1 >>= reduce1
Just (App (Lambda "y" (Lambda "x" (Lambda "y" (Var "y")))) (Lambda "x" (Lambda "y" (Var "x"))))
*Main> almostFalse1 >>= reduce1 >>= reduce1 
Just (Lambda "x" (Lambda "y" (Var "y")))
-}
almostTrue1 = reduce1 (App (App or' true') false')
{-
*Main> almostTrue1
Just (App (Lambda "v" (App (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Lambda "y" (Var "x")))) (Var "v"))) (Lambda "x" (Lambda "y" (Var "y"))))
*Main> almostTrue1 >>= reduce1
Just (App (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Lambda "y" (Var "x")))) (Lambda "x" (Lambda "y" (Var "y"))))
almostTrue1 >>= reduce1 >>= reduce1
Just (App (Lambda "y" (Lambda "x" (Lambda "y" (Var "x")))) (Lambda "x" (Lambda "y" (Var "y"))))
*Main> almostTrue1 >>= reduce1 >>= reduce1 >>= reduce1
Just (Lambda "x" (Lambda "y" (Var "x")))
-}

--Ex.0.2
--1
true1 = reduce (App (App and' true') true')
false1 = reduce (App (App or' false') false')
false2 = reduce (App not' true')
{-
*Main> true1
Lambda "x" (Lambda "y" (Var "x"))
*Main> false1
Lambda "x" (Lambda "y" (Var "y"))
*Main> false2
Lambda "x" (Lambda "y" (Var "y"))
-}

--2
zero = Lambda "f" (Lambda "x" x)

succ' = Lambda "n"(Lambda "f" (Lambda "x" (App f (App (App n f) x))))

plus' = Lambda "n1" (Lambda "n2" (App (App n1 succ') n2))

mult' = Lambda "n1" (Lambda "n2" (Lambda "f" (App n1 (App n2 f))))

one = reduce (App succ' zero) --Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))
two = reduce (App succ' one) --Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x"))))
three = reduce (App (App plus' one) two) --Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
six = reduce (App (App mult' two) three) --Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))))

--3
iszero = Lambda "n" (App (App n (Lambda "x" false')) true')

true2 = reduce (App iszero zero)
{-
*Main> true2
Lambda "x" (Lambda "y" (Var "x"))
-}

pred' = Lambda "n" (Lambda "f" (Lambda "x" (App (App (App n (Lambda "g" (Lambda "h" (App h (App g f)))))(Lambda "u" x)) (Lambda "u" u))))
{-
*Main> reduce (App pred' six)
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))
-}

minus' = Lambda "n1" (Lambda "n2" (App (App n1 pred') n2))
{-
*Main> reduce (App (App minus' three) six)  
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
-}

leq' = Lambda "n1" (Lambda "n2" (App iszero (App (App minus' n1) n2)))
{-
*Main> reduce (App (App leq' six) three)
Lambda "x" (Lambda "y" (Var "x"))
-}

eq' = Lambda "n1" (Lambda "n2" (App (App and' (App (App leq' n1) n2)) (App (App leq' n2) n1)))
{-
*Main> reduce (App (App eq' six) six)
Lambda "x" (Lambda "y" (Var "x"))

*Main> reduce (App (App eq' six) three)
Lambda "x" (Lambda "y" (Var "y"))
-}

--4

pair' = Lambda "x" (Lambda "y" (Lambda "z" (App (App z x) y)))
fst' = Lambda "p" (App p (Lambda "x" (Lambda "y" x)))
snd' = Lambda "p" (App p (Lambda "x" (Lambda "y" y)))

true_false = App (App pair' true') false'
{-
*Main> reduce (App fst' true_false)
Lambda "x" (Lambda "y" (Var "x"))
*Main> reduce (App snd' true_false)
Lambda "x" (Lambda "y" (Var "y"))
-}

--5

cons = pair'
head' = fst'
tail' = snd'
nil = false'
isnil = Lambda "l" (App (App l (Lambda "h" (Lambda "t" (Lambda "d" false')))) true')


six' = App (App cons six) nil
three_six = App (App cons three) six'
two_three_six = App (App cons two) three_six

{-
*Main> reduce (App isnil nil)
Lambda "x" (Lambda "y" (Var "x"))

*Main> reduce (App isnil two_three_six)
Lambda "x" (Lambda "y" (Var "y"))

*Main> reduce (App head' two_three_six)
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x"))))   --2

*Main> reduce (App head'(App tail' two_three_six)) 
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))   --3

*Main> reduce (App head'(App tail'(App tail' two_three_six)))
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))))   --6
-}