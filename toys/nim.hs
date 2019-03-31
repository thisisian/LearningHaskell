{-# OPTIONS_GHC -Wall #-}

import Data.List
import Data.Maybe
import Text.Read

-- Magic number indicating no move has been made
nulMove :: Int
nulMove = 0

main :: IO ()
main = do
    playGame startBoard 0 nulMove
    return ()
  where
    startBoard = replicate 5 5

playGame :: [Int] -> Int -> Int -> IO ()
playGame lstBoard turnCt lstMove =
    if gameOver
        then do
            putStr $ "Game over! Player " ++ show curPlayer ++ " wins!\n"
            return ()
        else do
            putStr $ showBoard curBoard ++ "\n"
            putStr prompt
            nextMove <- getLine
            let nextMove' = readMaybe nextMove
            if isNothing nextMove' || (not . isValid . fromJust $ nextMove')
                then do
                    putStr "Invalid lstMove!\n"
                    playGame curBoard turnCt nulMove
                else
                    playGame curBoard (turnCt + 1) (fromJust nextMove')
  where
    curPlayer = (turnCt `mod` 2) + 1
    prompt = "Player " ++ show curPlayer ++ ": "
    gameOver = all (==0) curBoard
    isValid n =
        n >= 1
        && n <= length lstBoard
        && lstBoard !! (n - 1) /= 0
    curBoard =
        if lstMove > 0
            then
                take (lstMove-1) lstBoard ++
                [pred $ lstBoard !! (lstMove-1)] ++
                drop lstMove lstBoard
            else
                lstBoard

showBoard :: [Int] -> String
showBoard x = concatMap showRow (zip x [1,2..])
  where
    showRow :: (Int, Int) -> String
    showRow (x', y') =
        show y' ++
        ": " ++ intersperse ' ' (replicate x' '*') ++
        "\n"
