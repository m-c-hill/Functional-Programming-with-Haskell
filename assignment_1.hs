-- M :: = x ∣ λx.M ∣ MM

-- pretty: a function that renders a Term as a lambda-term (but with \ for λ).

-- Var for variables, as a synonym of String
type Var = String

-- Term: data type for lambda-terms, with 3 constructors: Variable, Lambda and Apply.
-- These match the three cases of the definition of lambda terms: M :: = x ∣ λx.M ∣ MM
data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  -- deriving Show

instance Show Term where
  show = pretty

-- Example term
example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

-- Pretty renders a Term as a lambda-term (but with \ for λ)
pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

-- Example use:
-- *Main> putStrLn (pretty  example)
-- \a. \x. (\y. a) x b

------------------------- Assignment 1

numeral :: Int -> Term
numeral i = undefined


-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------- Assignment 2

variables :: [Var]
variables = undefined

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables = undefined

fresh :: [Var] -> Var
fresh = undefined

used :: Term -> [Var]
used = undefined


------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z) = undefined
rename x y (Lambda z n) = undefined
rename x y (Apply  n m) = undefined


substitute :: Var -> Term -> Term -> Term
substitute = undefined


------------------------- Assignment 4

beta :: Term -> [Term]
beta = undefined

normalize :: Term -> [Term]
normalize = undefined

normal :: Term -> Term
normal = undefined

-------------------------

a_beta :: Term -> [Term]
a_beta = undefined

a_normalize :: Term -> [Term]
a_normalize = undefined

a_normal :: Term -> Term
a_normal = undefined

-------------------------

example1 :: Term
example1 = undefined

example2 :: Term
example2 = undefined