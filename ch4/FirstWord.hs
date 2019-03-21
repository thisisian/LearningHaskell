-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile = do
  input <- readFile inputFile
  putStr (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> interactWith function input
            _ -> putStrLn "error: exactly one argument needed"

        -- replace "id" with the name of our function below
        myFunction = unlines.firstWords.splitLines

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)

firstWords :: [String] -> [String]
firstWords ([]:xs) = firstWords xs
firstWords (x:xs) = [head $ words x] ++ firstWords xs
firstWords _ = []

