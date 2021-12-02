--Problem 1: Lists

halving :: Int -> [Int]
halving n | n == 0 = []
          | odd n  = n : halving (n-1)
          | even n = n : halving (div n 2)

-- Collatz Sequences:
   -- If the number is even, divide it by two.
   -- If the number is odd, triple it and add one.

collatz :: Int -> [Int]
collatz n
      | n == 1 = [1]
      | odd n  = n : collatz (n*3 + 1)
      | even n = n : collatz (div n 2)

colLength :: Int -> Int
colLength n = length (collatz n)


-- Problem 2: Pattern Matching

maxList :: [Int] -> Int
maxList [] = 0
maxList [x] = x
maxList (x:xs)
      | (maxList xs) > x = maximum xs
      | otherwise = x


allDucks :: [String] -> Bool
allDucks [] = True
allDucks (x:xs)
      | x == "duck" = allDucks xs
      | otherwise = False

duckDuckGoose :: [String] -> Bool
duckDuckGoose [] = False
duckDuckGoose ["goose"] = True
duckDuckGoose (x:xs)
      | x == "duck" = duckDuckGoose xs
      | otherwise = False


-- Problem 3: Pairs

ducks :: [(String,Int)]
ducks = [("Donald",6),("Daisy",5),("Huey",2),("Louie",2),("Dewey",2)]

noDDucks :: [(String,Int)] -> [String]
noDDucks [] = []
noDDucks ((name,age):xs)
      | (head name) /= 'D' = name : noDDucks xs
      | otherwise = noDDucks xs

youngOrShort :: [(String, Int)] -> Bool
youngOrShort [] = False
youngOrShort ((name,age):xs)
      | (length name <= 3 || age < 3) = True
      | otherwise = youngOrShort xs

describeDucks :: [(String, Int)] -> String
describeDucks ((name,age):xs)
      | xs == [] = name ++ " is a duck who is " ++ show age ++ " years old."
      | otherwise = name ++ " is a duck who is " ++ show age ++ " years old. " ++ describeDucks xs
