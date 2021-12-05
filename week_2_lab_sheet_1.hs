import Data.Char

-- Example recursive functions from week 2 classes
snoc :: [Int] -> Int -> [Int]
snoc []     n = [n]
snoc (m:ms) n = m : snoc ms n

ourReverse :: [Int] -> [Int]
ourReverse []     = []
ourReverse (n:ns) = snoc (ourReverse ns) n

insertOrd :: Int -> [Int] -> [Int]
insertOrd m [] = [m]
insertOrd m (n:ns) | m <= n    = m : n : ns
                   | otherwise = n : (insertOrd m ns)

insertSort :: [Int] -> [Int]
insertSort []     = []
insertSort (x:xs) = insertOrd x (insertSort xs)

---Exercise 1: Recursive string functions

toUpperSt :: String -> String
toUpperSt [] = []
toUpperSt (c:cs) = toUpper c : toUpperSt cs

deleteDigits :: String -> String
deleteDigits [] = []
deleteDigits (x:xs)
        | isDigit x = deleteDigits xs
        | otherwise = x : deleteDigits xs

leetSpeak :: String -> String
leetSpeak [] = "!"
leetSpeak (x:xs)
        | x == 'e' = "7" ++ leetSpeak xs
        | x == 'o' = "0" ++ leetSpeak xs
        | x == 's' = "z" ++ leetSpeak xs
        | otherwise = x : leetSpeak xs


--Exercise 2

factors2 :: Int -> [Int]
factors2 0 = []
factors2 n | (n `mod` 2 == 0) =  2 : factors2 (n `div` 2)
           | otherwise        = [n]

factorsm :: Int -> Int -> [Int]
factorsm 0 _ = []
factorsm m n | (n `mod` m == 0) =  m : factorsm m (n `div` m)
             | otherwise        = [n]

factorsFrom :: Int -> Int -> [Int]
factorsFrom _ 0 = []  -- To handle non termination execution
factorsFrom 0 _ = []  -- To handle divide by zero exception
factorsFrom m n
        | m >= n           = [n]
        | (n `mod` m == 0) = m : factorsFrom (m+1) (n `div` m)
        | otherwise        = factorsFrom (m) n

primeFactors :: Int -> [Int]
primeFactors n = factorsFrom 2 n
