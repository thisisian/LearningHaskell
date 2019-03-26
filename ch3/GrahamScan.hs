{-# OPTIONS_GHC -Wall #-}

module GrahamScan where

import Data.List

data Point2D = Point2D {
                    xpos :: Float,
                    ypos :: Float
               }
               deriving (Eq)

instance Show Point2D where
    show pt = "(" ++ show (xpos pt) ++ ", " ++ show (ypos pt) ++ ")"

data Direction = LeftTurn | RightTurn | StraightTurn
    deriving (Show, Eq)

direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c = case compare 0 crossP of
                   GT -> LeftTurn
                   EQ -> StraightTurn
                   LT -> RightTurn
                where crossP = (xpos b - xpos a)*(ypos c - ypos a) -
                               (ypos b - ypos a)*(xpos c - xpos a)

-- Euler distance between a and b
eulDist :: Point2D -> Point2D -> Float
eulDist a b = sqrt ((xpos b- xpos a)**2 + (ypos b - ypos a)**2)

-- Compare polar angles of second and third arg relative to first argument.
-- Only valid for angles [0, pi]
angle :: Point2D -> Point2D -> Point2D -> Ordering
angle pt pta ptb
    | ypos pt == ypos pta && ypos pt == ypos ptb
                          = compare (xpos pta) (xpos ptb)
    | ypos pt == ypos pta = if xpos pta < xpos pt
                            then GT
                            else LT
    | ypos pt == ypos ptb = if xpos ptb < xpos pt
                            then LT
                            else GT
    | aAng <  bAng = LT
    | aAng > bAng  = GT
    | otherwise    = EQ
        where nInvSlope a b = -1 * ((xpos b-xpos a)/(ypos b-ypos a))
              aAng = nInvSlope pt pta
              bAng = nInvSlope pt ptb

hull :: [Point2D] -> [Point2D]
hull xs = reverse $ hull' xs []
              where hull' (pt:pts) stack
                           | length stack > 1 && findDirection (stack !! 1) (head stack) pt /= LeftTurn
                                       = hull' (pt:pts) (tail stack)
                           | otherwise = hull' pts (pt : stack)
                    hull' [] stack = stack

-- Find the point with lowest y value in first argument. Prefer a point with lowest x value.
lowest :: [Point2D] -> Point2D
lowest xs = foldr step (head xs) xs
                where step pt acc
                        | ypos pt < ypos acc  = pt
                        | ypos pt == ypos acc = if xpos pt < xpos acc then pt else acc
                        | otherwise           = acc

-- Remove points with duplicate polar angles relative to p. Prefer points farther away.
clean :: Point2D -> [Point2D] -> [Point2D]
clean p = foldr step []
                  where step x acc
                         | null acc  = [x]
                         | angle p x (head acc) == EQ
                                     = if eulDist p x > eulDist p (head acc)
                                       then x : tail acc
                                       else acc
                         | otherwise = x : acc

grahamScan :: [Point2D] -> [Point2D]
grahamScan xs
    | length xs < 3 = xs
    | otherwise    = reverse . hull . clean' $ sort' xs
        where y' = lowest xs
              clean' = clean y'
              sort' = sortBy (angle y')



