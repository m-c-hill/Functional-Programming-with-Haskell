-- Creating new data types, an example

data TwoStrings = TwoStr String String
st1, st2 :: TwoStrings  

st1 = TwoStr "One" "Two"
st2 = TwoStr "Alice" "Bob"

toPair :: TwoStrings -> (String,String)
toPair (TwoStr st1 st2) = (st1, st2)

toTwoStr :: String -> String -> TwoStrings
toTwoStr st1 st2 = TwoStr st1 st2


-- Types can have multiple constructors. For example:

data Day   = Mon | Tue | Wed | Thu | Fri | Sat | Sun
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug
   | Sep | Oct | Nov | Dec
data IntOrBool = I Int | B Bool

notNull :: IntOrBool -> Bool
notNull (I 0) = False
notNull (I _) = True
notNull (B b) = b

-- Variables as arguments for the type constructors.
data Two a = Two a a

twoNums :: Two Int
twoNums = Two 3 4

twoSts :: Two String
twoSts = Two "Jack" "Jill"

-- Creating New Types Out Of Old
divide :: Int -> Int -> Maybe Int
divide m 0 = Nothing  -- Haskell version of Null
divide m n = Just (m `div` n)  -- Just required since Nothing used


-- Lab sheet 2 exercises

data Duck = Duck String Int Float | Duckling String Int Float
        deriving Show  -- Displays duck information when constructed

donald :: Duck
donald = Duck "Donald" 6 0.63

daisy :: Duck
daisy = Duck "Daisy" 5 0.56

huey :: Duck
huey = Duck "Huey" 2 0.23

dewey :: Duck
dewey = Duck "Dewey" 2 0.25

duckFamily :: [Duck]
duckFamily = [donald,daisy,huey,dewey]

-- Exercise a
birthday :: Duck -> Duck
birthday (Duck name age height) = Duck name (age+1) height
birthday (Duckling name age height)
        | age + 1 >= 3 = Duck name (age+1) height
        | otherwise = Duckling name (age+1) height

-- Exercise b
-- Add height as a float to the Duck constructor (update birthday function to fit)

-- Exercise c
tall :: Duck -> Bool
tall (Duck name age height) = height > 0.6
tall (Duckling name age height) = height > 0.25

-- Exercise d
-- Create Duckling type (see above)

-- Exercise e
-- Add birthday and tall methods for duckling

-- Exercise f
-- Update duckling birthday method to convert to Duck at age 3