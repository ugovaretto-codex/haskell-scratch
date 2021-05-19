import           Data.Char      (digitToInt, isDigit)
import           System.CPUTime (getCPUTime)
import           System.IO      (hFlush, stdout)
import Control.Parallel

-- Board utilities:
type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row - 1) - num >= 0

move :: Board -> Int -> Int -> Board
move bs row num =
  let adjust r n =
        if r == row && valid bs r num then n - num else n
   in [adjust r n | (r, n) <- zip [1 ..] bs]

-- move b row num = zipWith (\r n -> if r == row then n-num else n) [1..] b

-- I/O utilities:

newline :: IO ()
newline = do
  putChar '\n'
  hFlush stdout

stars :: Int -> String
stars n = concat $ replicate n "* "

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr $ show row
  putStr ": "
  putStrLn $ stars num

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  hFlush stdout
  (x : _) <- getLine
  --newline
  if isDigit x
    then return (digitToInt x)
    else do
      newline
      putStrLn "ERROR: Invalid Digit!"
      getDigit prompt

-- Nim game:
nim :: IO ()
nim = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player =
  do
    newline
    putBoard board
    if finished board
      then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " WINS!!!"
      else do
        newline
        putStr "Player "
        print (show player)
        r <- getDigit "Enter a row number: "
        n <- getDigit "Enter stars to remove: "
        if valid board r n
          then play (move board r n) (next player)
          else do
            newline
            putStrLn "ERROR: Invalid Move!"
            play board player

--------------------------------------------------------------------------------
-- autoplay, AI

-- generate all possible moves: return list of (row, n) tuples
moves :: Board -> [(Int, Int)]
moves bs = [(row, n) | (b, row) <- zip bs [1 ..], n <- [1 .. b]]

-- generate all possible board combinations
boardMoves :: Board -> [Board]
boardMoves b = [move b row n | (row, n) <- moves b]

-- win condition: single row occupied, win by removed all stars
win :: Board -> Bool
win board = length (filter (/= 0) board) == 1

-- given board, return all possible winners: list with either '1' (player 1)
-- or '2' (player 2)
winners :: Board -> Int -> [Int]
winners board curPlayer
  | win board = [curPlayer]
  | otherwise = concat [winners b (next curPlayer) | b <- boardMoves board]

-- given board return number of times player one or two can win:
-- tuple (Int, Int)
winPlayers :: Board -> Int -> (Int, Int)
winPlayers board curPlayer = 
  let w = winners board curPlayer
      p1 = length $ filter (1 ==) w
      p2 = length $ filter (2 ==) w
   in (p1, p2)

-- select best move between two moves:
-- where number of wins for player 'curPlayer' are higher than for other player
type IT = (Int, Int)

selectBest :: (IT, IT) -> (IT, IT) -> Int -> (IT, IT)
selectBest (win1, move1) (win2, move2) curPlayer
  | curPlayer == 1 =
    let p1 = uncurry (-) win1
        p2 = uncurry (-) win2
     in if p1 > p2 then (win1, move1) else (win2, move2)
  | otherwise =
    let p1 = uncurry (-) win1
        p2 = uncurry (-) win2
     in if p1 > p2 then (win2, move2) else (win1, move1)

-- select best move among all possible moves
bestMove :: Board -> Int -> (Int, Int)
bestMove board curPlayer
  | finished board = (0, 0)
  | otherwise =
    let m = moves board
        w = [winPlayers (move board r n) (next curPlayer) | (r, n) <- m]
        (l : ls) = zip w m
     in snd $ foldl (\a b -> selectBest a b curPlayer) l ls

-- slow
wins :: Board -> Int -> (Int, Int) -> (Int, Int)
wins board player (p, q) | win board = if player == 1 then (p + 1, q) 
                           else (p, q+1)
                         | otherwise = foldl (\(a,b) (c, d) -> (a+c, b+d)) 
                                       (0,0) [wins b (next player) (p, q) 
                                              | b <- boardMoves board]

-- pickMove :: Board -> Int -> (Int, Int)
-- pickMove board player
--   | finished board = (0,0)
--   | otherwise =
--     let m = moves board
--         (l : ls) = zip (wins board player (0,0)) m
--      in snd $ foldl (\a b -> selectBest a b player) l ls

playerName :: Int -> String
playerName name
  | name == 1 = "Human"
  | otherwise = "HAL"

-- play against computer: it will always select the best move
-- (exhaustive search), impossible for a human to win
playAI :: Board -> Int -> IO ()
playAI board player =
  do
    newline
    putBoard board
    if finished board
      then do
        newline
        putStr "Player "
        putStr (playerName (next player))
        putStrLn " WINS!!!"
      else do
        newline
        putStr "Player "
        putStrLn (playerName player)
        if player == 1
          then do
            r <- getDigit "Enter a row number: "
            n <- getDigit "Enter stars to remove: "
            if valid board r n
              then playAI (move board r n) (next player)
              else do
                newline
                putStrLn "ERROR: Invalid Move!"
                playAI board player
          else do
            startTime <- getCPUTime
            let (row, n) = bestMove board player
            putStrLn $ "Row: " ++ show row ++ " N: " ++ show n
            finishTime <- getCPUTime
            print ("Time: " ++ show
                   (fromIntegral (finishTime - startTime) / 1000000000000)
                   ++ " s")
            playAI (move board row n) (next player)

nfib :: Int -> Int
nfib n | n <= 1 = 1
       | otherwise = par n1 (seq n2 (n1 + n2 + 1))
                     where n1 = nfib (n-1)
                           n2 = nfib (n-2)

main :: IO ()
main = print (wins [4,4,3,2,1] 1 (0,0))
