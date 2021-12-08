-- Var for variables, as a synonym of String
type Var = String

-- Term: data type for lambda-terms, with 3 constructors: Variable, Lambda and Apply.
-- These match the three cases of the definition of lambda terms: M :: = x ∣ λx.M ∣ MM
data Term =
    Variable Var        -- x
  | Lambda   Var  Term  -- λx.M
  | Apply    Term Term  -- MM
instance Show Term where
  show = pretty

-- Example term (\a. \x. (\y. a) x b)
example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

-- Pretty renders a Term as a lambda-term (but with \ for λ)
pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1: Church numerals

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

------------------------- Assignment 2: Generating fresh variables from a term

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

-- Collect all variable names from a term (lambda or variable) and returns ordered list of variables
used :: Term -> [Var]
used (Variable x) = [x]  -- Term: x (variable)
used (Lambda x m) = merge [x] (used m)  -- Term: λx.M (abstraction)
used (Apply  m n) = merge (used m) (used n)  -- Term: MN (application)


------------------------- Assignment 3: Capture-avoiding substitution

-- Renaming variables
rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x = Variable y
    | otherwise = Variable z
rename x y (Lambda z m)
    | z == x = Lambda z m
    | otherwise = Lambda z (rename x y m)
rename x y (Apply m n)
    = Apply (rename x y m) (rename x y n)

-- Capture-avoiding substitution
substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | y == x = n  -- This time, n is a variable rather than a name
    | otherwise = Variable y
substitute x n (Lambda y m)
    | y == x = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
        where
            z = fresh (merge [x] (merge (used n) (used m)))
substitute x n (Apply m_1 m_2)
    = Apply (substitute x n m_1) (substitute x n m_2)


------------------------- Assignment 4: Beta reduction

-- Beta reduction (normal order)

-- Returns a list of all beta-reducts of a term.
-- Following the beta-reduction logic detailed in the assignment, we apply beta to four pattern matching cases.
-- This includes the 3 variable patterns used previously and a top-level beta-step (λx.N) M →β N[M/x]. Either term within a term can be beta reduced (defined below recursively).
beta :: Term -> [Term]
beta (Apply (Lambda x n) m) = [substitute x m n] ++ [Apply (Lambda x b) m  | b <- (beta n)] ++ [Apply (Lambda x n)  a | a <- (beta m)]
beta (Apply n m) = [Apply b m  | b <- (beta n)] ++ [Apply n  a | a <- (beta m)]
beta (Lambda x n) = [Lambda x b | b <- (beta n)]
beta (Variable x) = []

-- List of all beta-reduction steps which leads from the initial term to a final normal form
normalize :: Term -> [Term]
normalize n
    | null (beta n) = [n]
    | otherwise = n : normalize (head (beta n))

-- Return the normal form of term n by retrieving the last element of list produced by apply `normalize` to n.
-- NOTE: Unsure how to tell if the list returned by normalize is infinite (Halting problem?)
-- There may be infinite beta steps if, for example, the reduction is caught in a loop. In situations such as this, there will be no normal form, however the below function will not terminate. May be able to add a timeout clause?
normal :: Term -> Term
normal n = last (normalize n)


-------------------------

-- Beta reduction (applicative order)

-- The above functions currently apply normal order beta reduction.
-- In order to apply applicative order beta reduction, a_beta can be the same as beta, as this is simply finding all possible beta-reducts.
-- However, the selection process (applied in a_normalize) will need to be updated.

a_beta :: Term -> [Term]
a_beta (Apply (Lambda x n) m) = [substitute x m n] ++ [Apply (Lambda x b) m  | b <- (a_beta n)] ++ [Apply (Lambda x n)  a | a <- (a_beta m)]
a_beta (Apply n m) = [Apply b m  | b <- (a_beta n)] ++ [Apply n  a | a <- (a_beta m)]
a_beta (Lambda x n) = [Lambda x b | b <- (a_beta n)]
a_beta (Variable x) = []

-- List of all beta-reduction steps which leads from the initial term to a final normal form
a_normalize :: Term -> [Term]
a_normalize n
    | null (beta n) = [n]
    | otherwise = n : a_normalize (last (a_beta n))  -- Now select last term in reduct list

a_normal :: Term -> Term
a_normal n = last (a_normalize n)

-------------------------

-- As seen in week 4's tutorial, the following term reduces with fewer steps using applicative-order reduction:
-- (\f. \x. f (f x)) (\a. (\x. x) ((\y. y) ((\z. z) a)))
example1 :: Term
example1 = Apply (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Variable"f") (Variable "x"))))) (Lambda "a" (Apply (Lambda "x" (Variable "x")) (Apply (Lambda "y" (Variable "y")) (Apply (Lambda "z" (Variable "z")) (Variable "a")))))

-- The term (\x. \y. y) ((\x. x) (\x. x)) (\x. x) will reduce to normal form in fewer steps when applying normal-order reduction.
example2 :: Term
example2 = (Apply (Apply (Lambda "x" (Lambda "y" (Variable "y"))) (Apply (Lambda "x" (Variable "x")) (Lambda "x" (Variable "x")))) (Lambda "x" (Variable "x")))
