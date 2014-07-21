module Prettify where

data Doc =
    Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Show, Eq)

-- Constructor functions
--
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

-- Data modification functions
--
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty  = x
x <> y = x `Concat` y

concat :: [[a]] -> [a]
concat = foldr (++) []

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line            = Char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

-- Exposed pretty function
--
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where
        best col (d:ds) =
            case d of
                Empty           -> best col ds
                Char c          -> c : best (col + 1) ds
                Text s          -> s ++ best (col + length s) ds
                Line            -> '\n' : best 0 ds
                a `Concat` b    -> best col (a:b:ds)
                a `Union` b     -> nicest col (best col (a:ds)) (best col (b:ds))
        best _ _ = ""
        nicest col a b
            | (width - least) `fits` a  = a
            | otherwise                 = b
            where
                least = min width col

-- Helper methods
--
fits :: Int -> String -> Bool
w `fits` _ | w < 0  = False
w `fits` ""         = True
w `fits` ('\n':_)   = True
w `fits` (c:cs)     = (w - 1) `fits` cs

