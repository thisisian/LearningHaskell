type Birds = Int
type Pole = (Int, Int)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
        | abs (l + n - r) < 4 = Just (l+n, r)
        | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
        | abs (r + n - l) < 4 = Just (l, r + n)
        | otherwise = Nothing


routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    return (start)

x -: f = f x