{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where
import Data.List

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

trans :: Matrix -> Matrix
trans [] = []
trans ([]:_) = [] -- [[],[],[]...] case
trans m = map head m : trans (map tail m)


allSeq :: Eq a => [a] -> [[a]]
allSeq [] = []
allSeq [x] = [[x]]
allSeq (x:xs) = let (l,r) = span (==x) (x:xs)
                in l : allSeq r

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
leftDiag m = head (head m) : leftDiag (tailRows (tailCols m))

rightDiag :: Matrix -> Row
rightDiag [] = []
rightDiag m = last (head m) : rightDiag (tailRows (initCols m))

winners :: Eq a => [a] -> [[a]]
winners ws = filter (\xs -> length xs == win) $ allSeq ws
--seqs = 
--map same b ++ map same (trans b) ++ map same leftDiag b ++ map same rightDiag b
--finished :: Board -> Player 
--finished [] = True
--finished b = 

--
main :: IO ()
main = print ""