
module Main where

-- generic
type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

blank :: Grid
blank = replicate 9 $ replicate 9 '.'

superEasy :: Grid
superEasy = [".6.3..8.4",
             "537.9....",
             ".4...63.7",
             ".9..51238",
             ".........",
             "71362..4.",
             "3.64...1.",
             "....6.523",
             "1.2..9.8."]

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols m = let l = length (head m)
         in [[r !! n | r <- rows m] | n <- [0..l-1]]

range :: Int -> Int -> [a] -> [a]
range _ _ [] = []
range l n x = take n (drop l x)

boxs :: Int -> Matrix a -> [Row a]
boxs n m = let l = length (head m) - n
               c = [range col n x | row <- [0,n..l],
                    col <- [0,n..l], x <- rows (range row n m)]
           in [concat (range i n c) | i <- [0, n..length c - 1]]

blankVal :: Value
blankVal = '.'

nodups :: [Value] -> Bool
nodups [] = True
nodups (x:xs) = if x == blankVal then nodups xs 
                else x `notElem` xs && nodups xs

type Choices = [Value]
choices :: Grid -> Matrix Choices
choices   = map (map choice)  where
               choice v = if v == '.' then ['1'..'9']
                          else [v]
collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ y : ys | y <- xs, ys <- cp xss]

---- domain
valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs 3 g)

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- entry point
main :: IO ()
main = do
          let solutions = solve superEasy
          print $ take 1 solutions