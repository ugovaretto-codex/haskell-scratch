
module Main where
import           Data.List
-- generic
type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols m = let l = length (head m)
         in [[r !! n | r <- rows m] | n <- [0..l-1]]

range :: Int -> Int -> [a] -> [a]
range _ _ [] = []
range l n x  = take n (drop l x)

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
cp []       = [[]]
cp (xs:xss) = [ y : ys | y <- xs, ys <- cp xss]

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m) &&
         all consistent (cols m) &&
         all consistent (boxs 3 m)

consistent :: Row Choices -> Bool
consistent []       = True
consistent ([x]:xs) = [x] `notElem` xs && consistent xs
consistent (x:xs)   = consistent xs

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

solve4 = search . prune . choices

search :: Matrix Choices -> [Grid]
search m | blocked m = []
     | all (all single) m = collapse m
     | otherwise = [g | m' <- expandFirst m,
                        g <- search (prune m')]

expandRow :: Row Choices -> [Row Choices]
expandRow [] = []
expandRow xss = let (lss, xs:rss) = span single xss
                in [lss ++ [[r]] ++ rss| r <- xs]

expandFirst :: Matrix Choices -> [Matrix Choices]
expandFirst [] = []
expandFirst m = let (lss, xs:rss) = span (all single) m
           in [lss ++ [r] ++ rss | r <- expandRow xs]


---- domain
valid :: Grid -> Bool

--- Boards
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


easy :: Grid
easy =  ["2....1.38",
         "........5",
         ".7...6...",
         ".......13",
         ".981..257",
         "31....8..",
         "9..8...2.",
         ".5..69784",
         "4..25...."]

--First gentle example from sudoku.org.uk:

gentle :: Grid
gentle = [".1.42...5",
          "..2.71.39",
          ".......4.",
          "2.71....6",
          "....4....",
          "6....74.3",
          ".7.......",
          "12.73.5..",
          "3...82.7."]

--First diabolical example:

diabolical :: Grid
diabolical = [".9.7..86.",
              ".31..5.2.",
              "8.6......",
              "..7.5...6",
              "...3.7...",
              "5...1.7..",
              "......1.9",
              ".2.6..35.",
              ".54..8.7."]

--First "unsolvable" (requires backtracking) example:

unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

--Minimal sized grid (17 values) with a unique solution:

minimal               :: Grid
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

---

--valid _ = True
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs 3 g)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy (boxs 3) . pruneBy cols . pruneBy rows where
        pruneBy f = f . map reduce . f

singles :: [[a]] -> [a]
singles xs = concat (filter single xs)

diff :: Eq a => [a] -> [a] -> [a]
diff [] _  = []
diff xs [] = xs
diff xs ys = [x | x <- xs, x `notElem` ys]

reduce :: Row Choices -> Row Choices
reduce xss = reduce' (singles xss) xss where
     reduce' _  []  = []
     reduce' ss xss = [if single xs then xs else diff xs ss | xs <- xss]

single :: [a] -> Bool
single []  = False
single [x] = True
single _   = False

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

solve2 = filter valid . collapse . prune . choices

solve3 = filter valid . collapse . fix prune . choices

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x

-- entry point
main :: IO ()
main = do
          let sol = take 1 $ solve3 easy
          print sol
