
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

genChoices :: Value -> Value -> Row Value -> Grid
genChoices x v xs = genChoices' x v ([],xs) where
     genChoices' x v ([],[]) = []
     genChoices' x v (l,[]) = []
     genChoices' x v (ls,r:rs) | r == v = (ls ++ [x] ++ rs) 
                                          : genChoices' x v (ls ++ [r],rs)
                               | otherwise = genChoices' x v (ls ++ [r],rs)

genElements :: [[a]] -> [a] -> [[a]]
genElements es xs = genElements' es ([],xs) where
     genElements' _ (_,[]) = []
     genElements' (c:cs) (ls,r:rs) = [ls ++ [x] ++ rs | x <- c] 
                                     ++ genElements' cs (ls ++ [r], rs)

genAll :: Value -> Value -> Grid -> [Grid]
genAll x v g = genElements (map (genChoices x v) g) g

genAllChoices :: Grid -> [Grid]
genAllChoices g = [gs | x <- ['1'..'9'], gs <- genAll x '.' g]

--warning: assumes grid is valid!
finished :: Grid -> Bool 
finished = all (notElem '.')

solve :: Grid -> [Grid]
solve [] = []
solve g | finished g = [g]
        | otherwise = concat [solve gs | gs <- genAllChoices g, valid gs]

---- domain
valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs 3 g)

-- entry point
main :: IO ()
main = do
          let sol = solve superEasy
          print (take 1 sol)