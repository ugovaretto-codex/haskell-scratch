
module Main where
-- generic
type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

blank :: Grid
blank = replicate 9 $ replicate 9 '.'

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
nodups (x:xs)  = if x == blankVal then nodups xs
                 else notElem x xs && nodups xs

-- type Grid = [[Char]]
-- cols = transpose

-- domain
valid :: Grid -> Bool 
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs 3 g)


main :: IO ()
main = print ""