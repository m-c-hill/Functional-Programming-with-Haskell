import Data.Char
import Control.Applicative (liftA2)

-- Examples from week 3

-- Map
addOneMap :: [Int] -> [Int]
addOneMap ns = map (+1) ns

lengthsMap :: [String] -> [Int]
lengthsMap sts = map length sts

addOneMap' :: [Int] -> [Int]
addOneMap' = map (+1)

lengthsMap' :: [String] -> [Int]
lengthsMap' = map length

-- Filter
longListsFil :: [[a]] -> [[a]]
longListsFil xs = filter long xs
    where long l = length l >= 5

evensFil' :: [Int] -> [Int]
evensFil' = filter even

-- Zip ([a] -> [b] -> [(a,b)])
addPair :: (Int,Int) -> Int
addPair (m,n) = m+n

addLists :: [Int] -> [Int] -> [Int]
addLists ms ns = map addPair (zip ms ns)

-- zipWith: used to combine map and zip
addListsZW :: [Int] -> [Int] -> [Int]
addListsZW ms ns = zipWith (+) ms ns

addListsZW' :: [Int] -> [Int] -> [Int]
addListsZW' = zipWith (+)

ordered :: [Int] -> Bool
ordered ns = and (zipWith (<=) ns (tail ns)) -- and returns true if all booleans in a list are true


-- Lab sheet exercises

--Maps
toLowerSt :: String -> String
toLowerSt = map toLower

toLowerCons :: Char -> Char
toLowerCons letter
        | letter `elem` ['A', 'E', 'I', 'O', 'U'] = letter
        | otherwise = toLower letter

toLowerConsSt :: String -> String
toLowerConsSt = map toLowerCons

--Filters
onlyLetters :: String -> String
onlyLetters = filter isLetter

onlyNumsOrLetters :: String -> String
onlyNumsOrLetters = filter (numOrLetter)
        where numOrLetter c = isDigit c || isLetter c

onlyLettersToLower1 :: String -> String
onlyLettersToLower1 st = map toLower (filter isLetter st)

onlyLettersToLower2 :: String -> String
onlyLettersToLower2 st = filter isLetter (map toLower st)

--Zips
firstNames :: [String]
firstNames = ["Adam","Brigitte","Charlie","Dora"]

secondNames :: [String]
secondNames = ["Ashe","Brown","Cook","De Santis"]

wholeNames :: [(String,String)]
wholeNames = zip firstNames secondNames

countNames :: [String] -> [(Int, String)]
countNames = zip [1..]

wholeNames2 :: [String]
wholeNames2 = zipWith (++) firstNames secondNames

rollCall :: [String] -> [String]
rollCall = zipWith call xs
      where call name x = name ++ ":" ++ x ++ "? 'Present!'"
            xs          = map show [1..]
