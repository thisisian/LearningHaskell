data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons a b) = a : toList b
