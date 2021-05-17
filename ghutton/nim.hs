import Data.Char

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
newline = putChar '\n'

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
  x <- getChar
  newline
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
        putStrLn (show player)
        r <- getDigit "Enter a row number: "
        n <- getDigit "Enter stars to remove: "
        if valid board r n
          then play (move board r n) (next player)
          else do
            newline
            putStrLn "ERROR: Invalid Move!"
            play board player

-- autoplay
moves :: Board -> [(Int, Int)]
moves bs = [(row, n) | (b, row) <- zip bs [1 ..], n <- [1 .. b]]

selectMove :: Board -> (Int, Int)
selectMove b =
  let ((row, n) : xs) = moves b
   in (row, n)

playGame :: Board -> Int -> IO ()
playGame board player = do
  putBoard board
  if finished board
    then do
      putStrLn ("Player: " ++ show (next player) ++ " WINS!")
    else
      let (row, n) = selectMove board
          b = move board row n
       in do
            newline
            print ("Player: " ++ show player)
            print ("Move: " ++ show row ++ ", -" ++ show n)
            putBoard b
            putStrLn (replicate 20 '=')
            newline
            playGame b (next player)