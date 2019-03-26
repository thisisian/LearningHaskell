import Data.Char
import Data.List
import Data.Maybe

safe :: Foldable t => (t a1 -> a2) -> t a1 -> Maybe a2
safe f xs = if null xs
            then Nothing
            else Just (f xs)

safeHead :: [a] -> Maybe a
safeHead xs = safe head xs

safeTail :: [a] -> Maybe [a]
safeTail xs = safe tail xs

safeLast :: [a] -> Maybe a
safeLast xs = safe last xs

safeInit :: [a] -> Maybe [a]
safeInit xs = safe init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs
    | null $ tail r     = [ys]
    | null ys = splitWith f (tail r)
    | otherwise  = [ys] ++ splitWith f (tail r)
    where (ys, r) = break f xs

asInt_fold :: String -> Int
asInt_fold ('-':[]) = error "Error parsing input"
asInt_fold [] = error "Error parsing input"
asInt_fold xs
    | head xs == '-'  = -1 * (asInt $ tail xs)
    | otherwise       = asInt xs
    where asInt xs = foldl f 0 xs
          f acc x 
                | isDigit x = acc * 10 + digitToInt x
                | otherwise = error "Error parsing input"
                        
test a b c
    | a == 1 = b * c
    | a == 2 = b * c
        where b = 10
              c = 4

asInt_either :: String -> Either String Int
asInt_either ('-':[]) = Left "No text following '-'"
asInt_either [] = Left "Empty input"
asInt_either xs 
            = either Left (Right . (foldl f 0)) (check (not.isDigit) xs')
                where f acc x = acc * 10 + a * digitToInt x
                      xs' = if head xs == '-'
                            then tail xs
                            else xs
                      a = if head xs == '-'
                          then -1
                          else 1

check :: Show a => (a -> Bool) -> [a] -> Either String [a]
check p xs = if isNothing invalid
             then Right xs
             else Left $ "Invalid data: " ++ (show $ fromJust invalid)
             where invalid = find p xs

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs)
    | p x == True = x : myTakeWhile p xs
    | otherwise = []
myTakeWhile _ _ = []

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 f xs = foldr step [] xs
                    where step x ys = if f x then x : ys else [] 

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f xs = foldr step [] xs
                            where step x [] = [[x]]
                                  step x acc  = if f x (head (head (acc)))
                                                then ([x] ++ head acc) : tail acc
                                                else [x] : acc

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr step False
        where step x False = p x
              step x True  = True

myCycle :: [a] -> [a]
myCycle xs = foldr step (myCycle xs) xs
             where step x acc = x : acc 

-- Bug: Will have leading empty string if there is leading whitespace
myWords :: String -> [String]
myWords = foldr step []
          where step x []      = if isSpace x then [] else [[x]]
                step x (y:ys)
                   | isSpace x = if null y 
                                     then (y:ys) 
                                     else []:(y:ys)
                   | otherwise = (x:y):ys

myUnlines :: [String] -> String
myUnlines xs = foldr step [] xs
               where step x acc = (x ++ "\n") ++ acc
