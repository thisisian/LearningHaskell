module Prettify where

import Prelude hiding ((<>))
import Data.List (replicate)
import Numeric

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = Concat x y

empty :: Doc
empty = Empty

text :: String -> Doc
text ""  = Empty
text str = Text str

double :: Double -> Doc
double d = text (show d)

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

fsep :: [Doc] -> Doc
fsep = foldr (</>) empty

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

softline :: Doc
softline = group line

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

--fill :: Int -> Doc -> Doc
--fill w x = hcat (fill' 0 [x])
--           where fill col []     = empty
--                 fill col (x:xs) = case of x
--                    | a `Concat` b -> fill 
--                    | Line -> (text $ replicate (w - col) ' ') : Line
--                    | other -> other

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)