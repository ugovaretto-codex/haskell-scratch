{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main where

import Data.List
import SortedTree

rows :: Int
rows = 6

cols :: Int
cols = 7

depth :: Int
depth = 4

win :: Int
win = 4

data Player = O | B | X deriving (Show, Ord, Eq)

type Matrix = [Row]

type Board = [Row]

type Row = [Player]

type Column = [Player]

emptyBoard :: Board
emptyBoard = replicate cols $ replicate rows B

initial :: Board
initial = replicate cols []

trans :: Matrix -> Matrix
trans [] = []
trans [[x]] = [[x]]
trans ([] : _) = [] -- [[],[],[]...] case
trans m = map head m : trans (map tail m)

allCells :: Matrix -> [Row]
allCells [] = []
allCells m = allDiags m ++ m ++ trans m

fillMat :: Int -> Matrix -> Matrix
fillMat _ [] = []
fillMat n m =
  [ xs
      ++ ( if n - length xs > 0
             then replicate (n - length xs) B
             else []
         )
    | xs <- m
  ]

tailCols :: Matrix -> Matrix
tailCols = trans . tail . trans

initCols :: Matrix -> Matrix
initCols = map init

tailRows :: Matrix -> Matrix
tailRows = tail

headCol :: Matrix -> Row
headCol = head . trans

headRow :: Matrix -> Row
headRow = head

lastCol :: Matrix -> Row
lastCol = map last

leftDiag :: Matrix -> Row
leftDiag [] = []
leftDiag [[x]] = [x]
leftDiag m = head (head m) : leftDiag (tailRows (tailCols m))

rightDiag :: Matrix -> Row
rightDiag [] = []
rightDiag [[x]] = [x]
rightDiag m = last (head m) : rightDiag (tailRows (initCols m))

winners :: Eq a => Int -> [a] -> [[a]]
winners _ [] = []
winners w ws = filter (\xs -> length xs == w) (allSeq ws)

appSeq :: ([a] -> [a]) -> [a] -> [[a]]
appSeq _ [] = []
appSeq f [x] = [f [x]]
appSeq f (x : xs) = f (x : xs) : appSeq f xs

allDiags :: Matrix -> [Row]
allDiags [] = []
allDiags m =
  map leftDiag (appSeq id m)
    ++ map rightDiag (appSeq id m)
    ++ map leftDiag (m : appSeq tailCols m)
    ++ map rightDiag (m : appSeq tailCols m)

allSeq :: Eq a => [a] -> [[a]]
allSeq [] = []
allSeq [x] = [[x]]
allSeq (x:xs) = let (l,r) = span (==x) (x:xs)
                in l : allSeq r

allCells :: Matrix -> [Row]
allCells [] = []
allCells m = allDiags m ++ m ++ trans m

tag :: Matrix -> Player
tag [] = B
tag m =
  let ws = filter (not . null) $ concatMap (winners win) $ allCells m
   in if null ws then B else head $ head ws

append :: Player -> Int -> Board -> Board
append p row b =
  let (lss, rs : rss) = splitAt row b
   in lss ++ [rs ++ [p]] ++ rss

boardAppend :: Player -> Int -> Board -> Board
boardAppend p col = append p (cols - col - 1)

toSym :: Player -> Char
toSym p
  | p == O = 'o'
  | p == X = 'x'
  | otherwise = '.'

fillm :: Matrix -> Matrix
fillm [] = []
fillm m = fillMat cols m

showBoard :: Board -> String
showBoard [] = ""
showBoard b =
  let bi = fillMat cols b
   in concat
        [ if null xs
            then ""
            else map toSym xs ++ "\n"
          | xs <- bi
        ]

choices :: Player -> Matrix -> [Matrix]
choices p m = choices' ([], m)
  where
    choices' (_, []) = []
    choices' (lss, rs : rss) =
      [ lss ++ [rs ++ [p]] ++ rss
        | length rs < cols
      ]
        ++ choices' (lss ++ [rs], rss)

search :: Player -> Int -> Matrix -> Player
search _ _ [] = B
search p d m
  | tag m `elem` [O, X] = tag m
  | d == 0 = tag m
  | otherwise =
    let t =
          treeSort
            (<)
            [ search (nextPlayer p) (d -1) x
              | x <- choices p m
            ]
     in if maxPlayer p then last t else head t

maxPlayer :: Player -> Bool
maxPlayer X = True
maxPlayer O = False

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

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

bestMove :: Player -> Board -> Int
bestMove p b =
  let moves =
        [ (search p depth $ fillm $ boardAppend p m b, m)
          | m <- [0 .. 6]
        ]
   in selectMove p moves

-- game

computer :: Player -> Bool
computer p
  | p == O = True
  | otherwise = False

play :: Player -> Board -> IO ()
play player board = do
  putStrLn $ showBoard (fillm board)
  let t = tag (fillm board)
  if t `elem` [X, O]
    then print ("Player " ++ show t ++ " WINS!")
    else
      if computer player
        then do
          print "Thinking..."
          let newBoard =
                boardAppend player (bestMove player board) board
          play (nextPlayer player) newBoard
        else do
          print "Select a column between 1 and 7"
          x <- getLine
          let c = (read x :: Int) - 1
          if c > 7 || c < 1
            then play player board
            else
              play
                (nextPlayer player)
                (boardAppend player c board)

main :: IO ()
main = play X initial
