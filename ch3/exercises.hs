{-# OPTIONS_GHC -Wall #-}

import Data.List

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome a = a ++ myReverse a

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == last xs  = isPalindrome (take (length xs - 1) xs)
    | otherwise     = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortOn length xs

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = x
intersperse' c (x:xs) = x ++ [c] ++ intersperse' c xs

data Tree a = Node a (Tree a) (Tree a) | Empty

treeHeight :: (Tree a) -> Int
treeHeight Empty = 0
treeHeight (Node _ l r)  = 1 + max (treeHeight l) (treeHeight r)

data Point2D = Point2D {
                    xpos :: Float,
                    ypos :: Float
               }

data Direction = LeftTurn | RightTurn | StraightTurn
    deriving (Show, Eq)

findDirection :: Point2D -> Point2D -> Point2D -> Direction
findDirection a b c = let crossP = ((xpos b - xpos a)*(ypos c - ypos a) -
                                    (ypos b - ypos a)*(xpos c - xpos a))
                      in if crossP > 0
                            then LeftTurn
                         else if crossP < 0
                            then RightTurn
                         else StraightTurn

findDirections :: [Point2D] -> [Direction]
findDirections (a:b:c:xs) = findDirection a b c : findDirections (b:c:xs)
findDirections _ = []

