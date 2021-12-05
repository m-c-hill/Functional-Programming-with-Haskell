---- Recursive data types

infiniteList :: [Int]
infiniteList = 1 : map (+1) infiniteList

--- Trees
-- Every binary tree is defined to be one of two things:
    -- An empty tree.
    -- A node, labelled with an Int, with a left binary subtree and a right binary subtree.
-- This definition is reflected below in the two different constructors for the IntTree (the latter being recursive)

data IntTree = Empty | Node Int IntTree IntTree

exampleTree :: IntTree
exampleTree = Node 1 (Node 0 Empty Empty) (Node 5 Empty Empty)

--- Recursive functions for trees

-- Find number of nodes in tree
size :: IntTree -> Int
size Empty = 0
size (Node node left right) = 1 + size left + size right

-- Find total of node values in a tree
total :: IntTree -> Int
total Empty = 0
total (Node n l r) = n + total l + total r

-- Map a function to every value in the tree
mapTree :: (Int -> Int) -> IntTree -> IntTree
mapTree f Empty = Empty
mapTree f (Node n l r) = Node (f n) (mapTree f l) (mapTree f r)

-- Eg. use it to double all values in tree
doubleTree :: IntTree -> IntTree
doubleTree = mapTree (*2)

--- Ordered trees
-- Function that inserts an integer into the correct place in an ordered tree to maintain its order.

insertT :: Int -> IntTree -> IntTree
insertT n Empty = Node n Empty Empty
insertT n (Node i l r) | n <= i = Node i (insertT n l) r
                       | n >  i = Node i l (insertT n r)

build :: [Int] -> IntTree
build []     = Empty
build (n:ns) = insertT n (build ns)

smallest :: IntTree -> Int
smallest Empty = error "No smallest element of an empty tree"
smallest (Node n Empty r) = n
smallest (Node n l r)     = smallest l

flatten :: IntTree -> [Int]
flatten Empty = []
flatten (Node n l r) = flatten l ++ [n] ++ flatten r


-- Lab sheet 2
t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty = 0
height (Node n l r) = 1 + max (height l) (height r)

find :: Int -> IntTree -> Bool
find y Empty = False
find y (Node n l r) = (y == n) || find y l || find y r

instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) =
          [ c:' ':m | m <- aux ' ' '|' s ] ++
          ['+':'-':show x] ++
          [ d:' ':n | n <- aux '|' ' ' t ]

------------------------- Exercise 2

member :: Int -> IntTree -> Bool
member _ Empty = False
member i (Node n l r)
        | i == n = True
        | n > i = member n l
        | n < i = member n r

largest :: IntTree -> Int
largest Empty = error "No largest element in an empty tree"
largest (Node n l Empty) = n
largest (Node n l r)     = largest r

deleteLargest :: IntTree -> IntTree
deleteLargest Empty = error "Empty tree"
deleteLargest (Node n l Empty) = l
deleteLargest (Node n l r) = Node n l (deleteLargest r)

delete :: Int -> IntTree -> IntTree
delete _ Empty = undefined
delete y (Node x l r)
        | y < x     = Node x (delete y l) r
        | y > x     = Node x l (delete y r)
        | isEmpty l = r
        | otherwise = Node (largest l) (deleteLargest l) r
