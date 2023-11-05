-- | Prettyify.hs

module Prettify
  (Doc, empty, hardline, char, text, double, integer, (<->), hcat, fsep, (</>), punctuate, compact, pretty)
where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Read,Show,Eq)
-- Primitives
-- Identity under concatenation
empty :: Doc
empty = Empty

hardline :: Doc
hardline = Line

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = empty
text s = Text s

double :: Double -> Doc
double = text . show

integer :: Integer -> Doc
integer = text . show

-- Composition
(<->) :: Doc -> Doc -> Doc
Empty <-> y = y
x <-> Empty = x
x <-> y = x `Concat` y

-- | Basic Concatenation Operation
--
-- Examples:
-- >>> text "foo" <-> text "bar"
-- Concat (Text "foo") (Text "bar")
--
-- >>> text "foo" <-> empty
-- Text "foo"
--
-- >>> empty <-> text "bar"
-- Text "bar"

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<->)

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other
group :: Doc -> Doc
group x = flatten x `Union` x
softline :: Doc
softline = group hardline
(</>) :: Doc -> Doc -> Doc
x </> y = x <-> softline <-> y
fsep :: [Doc] -> Doc
fsep = fold (</>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <-> p) : punctuate p ds

-- | More Concatenation operations
--
-- Examples:
--
-- >>> empty </> char 'a'
-- Concat (Union (Char ' ') Line) (Char 'a')

-- Coercion
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

          nicest col a b | (width - col) `fits` a = a
                         | otherwise                = b
            where w `fits` _ | w < 0 = False
                  _ `fits` ""        = True
                  _ `fits` ('\n':_)  = True
                  w `fits` (_:cs)    = (w - 1) `fits` cs


-- | Coercion
--
-- Examples:
--
-- >>> compact (char 'f' <-> text "oo")
-- "foo"
