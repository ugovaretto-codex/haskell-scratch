
module Main where
import Control.Monad.Trans.State ( runState, state, State, get )
import Debug.Trace
-- program
data Prog = Assign Name Expr
          | If Expr Prog Prog
          | While Expr Prog
          | Seqn [Prog] deriving Show
data Expr = Val Int | Var Name | App Op Expr Expr deriving Show
type Name = Char
data Op = Add | Sub | Mul | Div deriving Show
instance Eq Op where
  (==) Add Add = True
  (==) Sub Sub = True
  (==) Mul Mul = True
  (==) Div Div = True
  (==) _ _ = False 

-- virtual machine
type Stack = [Int]
type Mem = [(Name, Int)]
type Code = [Inst]
data Inst = PUSH Int
          | PUSHV Name
          | POP Name
          | DO Op
          | JUMP Label
          | JUMPZ Label
          | LABEL Label
          | END
          deriving (Show, Eq)
type Label = Int

-- Exercise 1
-- Define a function
-- comp :: Prog -> Code
-- that translates a program into machine code, using a state monad to handle 
-- the generation of fresh labels.  For example, the result ofcomp (fac 10)should
-- be as follows:
test = 
  [PUSH 1, POP 'A',
   PUSH 10, POP 'B',
    LABEL 0,
    PUSHV 'B', JUMPZ 1,
    PUSHV 'A', PUSHV 'B', DO Mul, POP 'A',
    PUSHV 'B', PUSH 1, DO Sub, POP 'B',
    JUMP 0,
   LABEL 1]

fac :: Int -> Prog
fac n = Seqn [Assign 'A' (Val 1),
              Assign 'B' (Val n),
              While (Var 'B') (Seqn
                [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))])]

newtype CompilerState = CS{label::Label}
nextLabel :: CompilerState -> CompilerState
nextLabel (CS l) = CS{label=l+1}

initState :: CompilerState
initState = CS{label = -1}

comp :: Prog -> Code
comp = fst . comp' initState

comp' :: CompilerState -> Prog -> (Code, CompilerState)
comp' cs (Assign name expr) = (compExpr expr ++ [POP name],cs)
comp' cs (If expr p1 p2) = let cs' = nextLabel cs
                               cs'' = nextLabel cs'
                               labelZero = label cs'
                               labelNotZero = label cs''
                               (c1,s1) = comp' cs'' p1
                               (c2,s2) = comp' s1 p2
                           in (compExpr expr ++ 
                               [JUMPZ labelZero] ++
                               c1 ++
                               [JUMP labelNotZero] ++
                               [LABEL labelZero] ++
                               c2 ++
                               [LABEL labelNotZero], s2)
comp' cs (While expr p) = let cs' = nextLabel cs
                              labelBegin = label cs'
                              cs'' = nextLabel cs'
                              labelEnd = label cs''
                              (c,s) = comp' cs'' p
                          in ([LABEL labelBegin] ++ 
                              compExpr expr ++ 
                              [JUMPZ labelEnd] ++ 
                              c ++
                              [JUMP labelBegin] ++ 
                              [LABEL labelEnd], s)
comp' cs (Seqn prog) = foldl (\(prev,s) p -> let (c,s') = comp' s p
                                             in (prev ++ c, s'))
                             ([],cs)
                             prog

compExpr :: Expr -> Code
compExpr (Val n) = [PUSH n]
compExpr (Var v) = [PUSHV v]
compExpr (App op ex1 ex2) = compExpr ex1 ++ compExpr ex2 ++ [DO op]

-- Tests

assignTest :: String
assignTest = let prog = Assign 'A' (Val 1)
             in show (comp prog)

assignOpTest :: String
assignOpTest = show $ comp (Assign 'A' (App Mul (Var 'A') (Var 'B')))

seqnTest :: String
seqnTest = show $ comp (Seqn [Assign 'A' (Val 1), Assign 'B' (Val 2)])

seqnNestTest :: String
seqnNestTest = show $ comp (Seqn [ 
                              Assign 'A' (Val 1),
                              Assign 'B' (Val 10),
                              While (Var 'B') (
                                Seqn [
                                  Assign 'A' (App Mul (Var 'A') (Var 'B')),
                                  Assign 'B' (App Sub (Var 'B') (Val 1))])])

ifTest :: String
ifTest = show $ comp (If (Var 'A') (Assign 'B' (Val 0)) (Assign 'B' (Val 1)))

whileTest :: String
whileTest = show $ comp (While (Var 'A') (Assign 'A' (Val 0)))

exercise1 :: Bool
exercise1 = test == comp (fac 10)

-- Ex 1 DONE, TBD: Monads

-- Exercise 2

type ES = (Stack, Code, Mem, Code)
type ExecState = State ES Inst

nextInst :: ES -> (Inst, ES)
nextInst (s,c,m,[]) = (END,(s,c,m,[]))
nextInst (stack, code, mem, i:ip) = (i, (stack, code, mem, ip))

push :: Int -> ES -> ES
push n (stack, code, mem, ip) = (n:stack, code, mem, ip)

pushv :: Name -> ES -> ES
pushv n (stack, code, mem, ip) = (val n mem : stack, code, mem, ip)

pop :: Name -> ES -> ES
pop _ ([], code, mem, ip) = ([], code, mem, ip)
pop n (s:stack, code, mem, ip) = (stack, code, 
                                  (n,s) : filter (\(m,_) -> n /=m ) mem,
                                  ip) 

doOp :: Op -> ES -> ES
doOp _ ([], c, m, ip) = ([], c, m, ip)
doOp _ ([x], c, m, ip) = ([x], c, m, ip)
doOp op (x:y:xs, c, m, ip) | op == Add = ((x+y):xs, c, m, ip)
                           | op == Mul = ((x*y):xs, c, m, ip)
                           | op == Sub = ((y-x):xs, c, m, ip)
                           | op == Div = ((x `div` y):xs, c, m, ip)
                           | otherwise = (x:y:xs, c, m, ip)

jump :: Label -> ES -> ES
jump l (s,c,m,i) = (s,c,m,skip (LABEL l) c)

jumpz :: Label -> ES -> ES
jumpz _ ([],c,m,ip) = ([],c,m,ip)
jumpz l (x:xs,c,m,ip) | x == 0 = jump l (xs,c,m,ip)
                      | otherwise = (xs,c,m,ip)

labelInst :: ES -> ES
labelInst = id

skip :: Eq a => a -> [a] -> [a]
skip _ [] = []
skip e (x:xs) | e == x = xs
              | otherwise = skip e xs


-- WARNING: Assumes element IS in list, if not use find with Maybe
val :: Name -> Mem -> Int
val n = foldl (\p (name, val) -> if name == n then val else p) 0

exec' :: ExecState
exec' = do
          inst <- state nextInst
          --s <- get
          --traceM (show s)
          case inst of
            (PUSH n) -> state (\s -> (PUSH n, push n s))
            (PUSHV n) -> state (\s -> (PUSHV n, pushv n s))
            (POP n) -> state(\s -> (POP n, pop n s))
            (DO op) -> state(\s -> (DO op, doOp op s))
            (JUMP lbl) -> state(\s -> (JUMP lbl, jump lbl s))
            (JUMPZ lbl) -> state(\s -> (JUMPZ lbl, jumpz lbl s))
            (LABEL lbl) -> state(\s -> (LABEL lbl, labelInst s))
            END -> pure END
          if inst /= END then exec' else pure END 
          
exec :: Code -> Mem
exec code = let (_,(_,_,mem,_)) = runState exec' ([],code,[],code)
            in mem

main :: IO ()
main = do
        print $ exec (comp (fac 10))           