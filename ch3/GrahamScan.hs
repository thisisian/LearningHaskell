{-# OPTIONS_GHC -Wall #-}

module GrahamScan where

import Data.List

data Point2D = Point2D {
                    xpos :: Float,
                    ypos :: Float
               }

instance Show Point2D where
    show pt = "(" ++ (show $ xpos pt) ++ ", " ++ (show $ ypos pt) ++ ")"

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

-- Euler distance between a and b
eulDist :: Point2D -> Point2D -> Float
eulDist a b = sqrt ((xpos b- xpos a)**2 + (ypos b - ypos a)**2)

-- Compare polar angles of second and third arg relative to first argument.
-- Only valid for angles [0, pi]
compareAngle :: Point2D -> Point2D -> Point2D -> Ordering
compareAngle pt pta ptb 
    | ypos pt == ypos pta && ypos pt == ypos ptb
                          = compare (xpos pta) (xpos ptb)
    | ypos pt == ypos pta = if xpos pta < xpos pt then GT else LT
    | ypos pt == ypos ptb = if xpos ptb < xpos pt then LT else GT
    | aAng <  bAng = LT
    | aAng > bAng  = GT
    | otherwise    = EQ
        where nInvSlope a b = -1 * ((xpos b-xpos a)/(ypos b-ypos a))
              aAng = nInvSlope pt pta
              bAng = nInvSlope pt ptb

findHull :: [Point2D] -> [Point2D]
findHull xs = findHull' xs []
              where findHull' (pt:pts) stack
                           | length stack > 1 && findDirection (stack !! 1) (head stack) pt == LeftTurn
                                       = findHull' (pt:pts) (tail stack)
                           | otherwise = findHull' pts (pt : stack)
                    findHull' [] stack = stack

-- Find the point with lowest y value in first argument. Prefer a point with lowest x value.
findLowest :: [Point2D] -> Point2D
findLowest xs = foldr step (head xs) xs
                where step pt acc 
                        | ypos pt < ypos acc  = pt
                        | ypos pt == ypos acc = if xpos pt < xpos acc then pt else acc
                        | otherwise           = acc

-- Remove points with duplicate polar angles relative to p. Prefer points farther away.
removeDups :: Point2D -> [Point2D] -> [Point2D]
removeDups p xs = foldr step [] xs
                  where step x acc 
                         | null acc  = x : acc
                         | compareAngle p x (head acc) == EQ
                                     = if (eulDist p x) > (eulDist p (head acc))
                                         then x : acc
                                         else []
                         | otherwise = x : acc

grahamScan :: [Point2D] -> [Point2D]
grahamScan xs = let y' = findLowest xs
                    sort' = sortBy (compareAngle y')
                in  findHull $ removeDups y' $ sort' xs

