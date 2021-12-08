import System.Random

-- Var for variables, as a synonym of String
type Var = String

-- Term: data type for lambda-terms, with 3 constructors: Variable, Lambda and Apply.
-- These match the three cases of the definition of lambda terms: M :: = x ∣ λx.M ∣ MM
data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
    --deriving Show

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

-- Example Church numerals:
-- 0
test_numeral_0 :: Term
test_numeral_0 = Lambda "f" (Lambda "x" (Variable "x"))

-- 1
test_numeral_1 :: Term
test_numeral_1 = Lambda "f" (Lambda "x" (Apply (Variable "f") (Variable "x")))

-- 2
test_numeral_2 :: Term
test_numeral_2 = Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Variable "f") (Variable "x"))))

-- Function to recursively generate a specific Church Numeral
numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (prev_numeral i))
    where
        prev_numeral :: Int -> Term
        prev_numeral i
            | i == 0 = Variable "x"
            | otherwise = Apply (Variable "f") (prev_numeral (i-1))

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

-- List comprehension used to generate infinite set of variables with naming convention 'char-num', where char is a single character and num is any number greater than 1 (includes a..z with no numbers initially).
-- Reference: https://wiki.haskell.org/Cookbook/Lists_and_strings
variables :: [Var]
variables = char_list ++ numbered_variables
    where
        char_list = map(:[]) ['a'..'z'] -- Initial alphabet with no numbers
        numbered_variables = [ a : show n | n <- [1..] , a <- ['a'..'z'] ]

-- x is the list to filter, y is the list of variable to filter out of x
filterVariables :: [Var] -> [Var] -> [Var]
filterVariables x y = [i | i <- x, i `notElem` y]

-- Generate a new variable not in var_list
fresh :: [Var] -> Var
fresh var_list = head (filterVariables variables var_list)

-- Collect all variable names from a term (lambda or variable) and returns ordered list of variables. Recursively
used :: Term -> [Var]
used (Variable x) = [x]  -- Term: x (variable)
used (Lambda x m) = merge [x] (used m)  -- Term: λx.M (abstraction)
used (Apply  m n) = merge (used m) (used n)  -- Term: MN (application)


------------------------- Assignment 3

-- Capture avoiding

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