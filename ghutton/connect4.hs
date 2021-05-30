module Main where

import Data.List
import System.Random

-- Entry point
main :: IO ()
main = play X initial

-- Data types & global config
-- Transposed board to make is easier to append to rows instead of columns
rows :: Int
rows = 3

cols :: Int
cols = 3

depth :: Int
depth = 8

win :: Int
win = 3

initial :: Board
initial = replicate rows []

data Player = O | B | X deriving (Show, Ord, Eq)

type Matrix = [Row]

type Board = [Row]

type Row = [Player]

type Column = [Player]

-- Play game
play :: Player -> Board -> IO ()
play player board = do
  putStrLn $ (showBoard . rotateLeft . fillm) board --print filled board
  let t = tag board -- check tag, if X or O somebody is the winner
  if t `elem` [X, O]
    then putStrLn ("Player " ++ show t ++ " WINS!")
    else
      if computer player
        then do
          putStrLn "Thinking..."
          let move = bestMove player board
          putStrLn ("column " ++ show (move + 1))
          let newBoard =
                append player move board
          play (nextPlayer player) newBoard
        else do
          putStrLn $ "Select a column between 1 and " ++ show rows
          x <- getLine
          let c = (read x :: Int) - 1
          if c > (rows - 1) || c < 0 || length (board !! c) >= cols
            then do
              putStrLn "Invalid row"
              play player board
            else do
              play
                (nextPlayer player)
                (append player c board)

rotateLeft :: Board -> Board
rotateLeft = trans . map reverse

computer :: Player -> Bool
computer p
  | p == O = True
  | otherwise = False

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

append :: Player -> Int -> Board -> Board
append p row b =
  let (lss, rs : rss) = splitAt row b
   in lss ++ [rs ++ [p]] ++ rss

maxPlayer :: Player -> Bool
maxPlayer X = True
maxPlayer O = False

-- utility
tails' :: [a] -> [[a]] -- standard tails ends with an empty list
tails' = init . tails

seed :: Int
seed = 40

generator = mkStdGen seed

randElement :: [a] -> a
randElement xs = xs !! rand
  where
    n = length xs
    (rand, _) = randomR (0, n -1) $ mkStdGen seed

-- I/O
showBoard :: Board -> String
showBoard [] = ""
showBoard b =
  concat
    [ if null xs
        then ""
        else map toSym xs ++ "\n"
      | xs <- b
    ]

fillm :: Matrix -> Matrix
fillm [] = []
fillm m =
  [ xs
      ++ ( if cols - length xs > 0
             then replicate (cols - length xs) B
             else []
         )
    | xs <- m
  ]

toSym :: Player -> Char
toSym p
  | p == O = 'o'
  | p == X = 'x'
  | otherwise = '.'

-- Look for winning sequences: diagonals, rows, columns
tag :: Matrix -> Player
tag [] = B
tag m =
  let ws = filter (not . null) $ concatMap (winners win) $ allCells (fillm m)
   in if null ws || null (head ws) then B else head $ head ws

winners :: Eq a => Int -> [a] -> [[a]]
winners _ [] = []
winners w ws = filter (\xs -> length xs == w) (allSeq ws)

allCells :: Matrix -> [Row]
allCells [] = []
allCells ([] : _) = []
allCells m = allDiags m ++ m ++ trans m

allSeq :: Eq a => [a] -> [[a]]
allSeq [] = []
allSeq [x] = [[x]]
allSeq (x : xs) =
  let (l, r) = span (== x) (x : xs)
   in l : allSeq r

-- diagonals :: [[a]] -> [[a]]
-- diagonals = tail . go []
--   where
--     -- it is critical for some applications that we start producing answers
--     -- before inspecting es_
--     go b es_ =
--       [h | h : _ <- b] : case es_ of
--         [] -> transpose ts
--         e : es -> go (e : ts) es
--       where
--         ts = [t | _ : t <- b]

allDiags :: Matrix -> [Row]
allDiags [] = []
allDiags m =
  map leftDiag (m : tails' m)
    ++ map rightDiag (m : tails' m)
    ++ map (leftDiag . tailCols) (m : tails' m)
    ++ map (rightDiag . tailCols) (m : tails' m)

-- needs dense matrix
trans :: Matrix -> Matrix
trans [] = []
trans ([] : xss) = trans xss
trans ((x : xs) : xss) = (x : [h | (h : _) <- xss]) : trans (xs : [t | (_ : t) <- xss])

leftDiag :: Matrix -> Row
leftDiag [] = []
leftDiag ([] : _) = []
leftDiag [[x]] = [x]
leftDiag m = head (head m) : leftDiag (tailRows (tailCols m))

rightDiag :: Matrix -> Row
rightDiag [] = []
rightDiag ([] : _) = []
rightDiag [[x]] = [x]
rightDiag m = last (head m) : rightDiag (tailRows (initCols m))

tailRows :: Matrix -> Matrix
tailRows [] = []
tailRows ([] : _) = []
tailRows [[_]] = []
tailRows m = tail m

tailCols :: Matrix -> Matrix
tailCols [] = []
tailCols ([] : _) = []
tailCols m = (trans . tailRows . trans) m

initCols :: Matrix -> Matrix
initCols = map init

full :: Matrix -> Bool
full [] = False
full m = all (\xs -> not (null xs) && all (\x -> x `elem` [O, X]) xs) m

-- AI
bestMove :: Player -> Board -> Int
bestMove p b =
  let moves =
        [ (search p depth $ append p m b, m)
          | m <- [0 .. rows - 1],
            length (b !! m) < cols
        ]
   in selectMove p moves

--
search :: Player -> Int -> Matrix -> Player
search _ _ [] = B
search _ _ ([] : _) = B
search p d m
  | tag m `elem` [O, X] = tag m
  | d == 0 = tag m
  | full m = B -- should get here only if draw i.e. full board no winner
  | otherwise =
    let t =
          [ search (nextPlayer p) (d -1) xs
            | xs <- choices p m
          ]
     in if maxPlayer p then foldr max O t else foldr min X t

-- if 'max' player select move with highest value, select move with lowest
-- value otherwise
selectMove :: Player -> [(Player, Int)] -> Int
selectMove p xs | maxPlayer p =
                    let (_, m) =
                          foldr
                            ( \(x1, y1) (x2, y2) ->
                                if x1 > x2
                                  then (x1, y1)
                                  else (x2, y2)
                            )
                            (O, 0)
                            xs
                    in m
                | otherwise =
                    let (_, m) =
                          foldr
                            ( \(x1, y1) (x2, y2) ->
                                if x1 < x2
                                  then (x1, y1)
                                  else (x2, y2)
                            )
                            (X, 0)
                            xs
                      in m

choices :: Player -> Matrix -> [Matrix]
choices p m = [append p i m | i <-[0 .. rows - 1], length (m !! i) < cols]