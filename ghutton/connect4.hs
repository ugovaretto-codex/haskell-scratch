module Main where

import Data.List
import SortedTree

-- Entry point
main :: IO ()
main = play X initial

-- Data types & global config
-- Transposed board to make is easier to append to rows instead of columns
rows :: Int
rows = 7

cols :: Int
cols = 6

depth :: Int
depth = 4

win :: Int
win = 4

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
  putStrLn $ showBoard board --print filled board
  let t = tag board -- check tag, if X or O somebody is the winner
  if t `elem` [X, O]
    then putStrLn ("Player " ++ show t ++ " WINS!")
    else
      if computer player
        then do
          putStrLn "Thinking..."
          let move = bestMove player board
          putStrLn ("column " ++ show move)
          let newBoard =
                append player move board
          play (nextPlayer player) newBoard
        else do
          putStrLn  $ "Select a row between 1 and " ++ show rows
          x <- getLine
          let c = (read x :: Int) - 1
          if c > (rows - 1) || c < 0 || length (board !! c) >= cols
            then play player board
            else
              play
                (nextPlayer player)
                (append player c board)

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

-- I/O
showBoard :: Board -> String
showBoard [] = ""
showBoard b =
  let bi = fillm b
   in concat
        [ if null xs
            then ""
            else map toSym xs ++ "\n"
          | xs <- bi
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
allCells m = allDiags m ++ m ++ trans m

allSeq :: Eq a => [a] -> [[a]]
allSeq [] = []
allSeq [x] = [[x]]
allSeq (x : xs) =
  let (l, r) = span (== x) (x : xs)
   in l : allSeq r

allDiags :: Matrix -> [Row]
allDiags [] = []
allDiags m =
  map leftDiag (appSeq id m)
    ++ map rightDiag (appSeq id m)
    ++ map leftDiag (m : appSeq tailCols m)
    ++ map rightDiag (m : appSeq tailCols m)

trans :: Matrix -> Matrix
trans [] = []
trans [[x]] = [[x]]
trans ([] : _) = [] -- [[],[],[]...] case
trans m = map head m : trans (map tail m)

leftDiag :: Matrix -> Row
leftDiag [] = []
leftDiag [[]] = []
leftDiag [[x]] = [x]
leftDiag m = head (head m) : leftDiag (tailRows (tailCols m))

rightDiag :: Matrix -> Row
rightDiag [] = []
rightDiag [[]] = []
rightDiag [[x]] = [x]
rightDiag m = last (head m) : rightDiag (tailRows (initCols m))

tailRows :: Matrix -> Matrix
tailRows [] = []
tailRows [[]] = []
tailRows [[_]] = []
tailRows m = tail m

tailCols :: Matrix -> Matrix
tailCols [] = []
tailCols [[]] = []
tailCols [[_]] = []
tailCols m = (trans . tail . trans) m

initCols :: Matrix -> Matrix
initCols = map init

appSeq :: ([a] -> [a]) -> [a] -> [[a]]
appSeq _ [] = []
appSeq f [x] = [f [x]]
appSeq f (x : xs) = f (x : xs) : appSeq f xs

full :: Matrix -> Bool
full [] = False
full m = all (\xs -> not (null xs) && all (\x -> x `elem` [O, X]) xs) m

-- AI
bestMove :: Player -> Board -> Int
bestMove p b =
  let moves =
        [ (search p depth $ append p m b, m)
          | m <- [0 .. rows - 1], length (b !! m) < cols
        ]
   in selectMove p moves

search :: Player -> Int -> Matrix -> Player
search _ _ [] = B
search p d m
  | tag m `elem` [O, X] = tag m
  | d == 0 = tag m
  | full m = B
  | otherwise =
    let t =
          [ search (nextPlayer p) (d -1) xs
            | xs <- choices p m
          ]
     in if null t
          then B
          else if maxPlayer p then foldr max O t else foldr min X t

selectMove :: Player -> [(Player, Int)] -> Int
selectMove p xs
  | maxPlayer p =
    let (_, x) =
          foldr
            ( \(x1, y1) (x2, y2) ->
                if x1 > x2
                  then (x1, y1)
                  else (x2, y2)
            )
            (O, 0)
            xs
     in x
  | otherwise =
    let (_, x) =
          foldr
            ( \(x1, y1) (x2, y2) ->
                if x1 < x2
                  then (x1, y1)
                  else (x2, y2)
            )
            (X, 0)
            xs
     in x

choices :: Player -> Matrix -> [Matrix]
choices p m = [append p i m | i <- [0 .. rows - 1]]