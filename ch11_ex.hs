import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)
import Debug.Trace

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

-- NOTE: doesn't work because of Ex4 a
-- turn :: Grid -> Player
-- turn g = if os <= xs then O else X
--   where
--     os = length (filter (== O) ps)
--     xs = length (filter (== X) ps)
--     ps = concat g

lineLength :: Int
lineLength = 3

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where line = (>= lineLength) . length . filter (== p) -- Ex4 b
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size*size && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prmt = do putStr prmt
                 xs <- getLine
                 if xs /= [] && all isDigit xs then
                   return (read xs)
                 else
                   do putStrLn "ERROR: Invalid number"
                      getNat prmt

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | otherwise =
    do i <- getNat (prompt p)
       case move g i p of
         [] -> do putStrLn "ERROR: Invalid move"
                  run' g p
         [g'] -> run g' (next p)
         _ -> undefined

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
  deriving (Show, Eq)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..((size*size)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Player -> Tree Grid -> Tree (Grid, Player)
minimax _ (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []

minimax p (Node g ts)
  | p == O = Node (g, minimum ps) ts'
  | p == X = Node (g, maximum ps) ts'
    where
      ts' = map (minimax (next p)) ts
      ps  = [p | Node(_, p) _ <- ts']

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = alphabeta p tree


play :: Tree Grid -> Grid -> Player -> IO ()
play t g p = do cls
                goto (1,1)
                putGrid g
                play' t g p

play' :: Tree Grid -> Grid -> Player -> IO ()
play' t g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "Error: Invalid move"
                             play' t g p
                    [g'] -> play (subtree t g') g' (next p)
  | p == X   = do putStr "Player X is thinking... "
                  -- g' <- randomElem (bestmoves g p)
                  let g' = (quickestmove t g p)
                  (play (subtree t g') $! g') (next p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          player <- askFirst
          play (gametree empty player) empty player

-- Ex1
tcount :: Tree a -> Int
tcount (Node _ ts) = 1 + sum (map tcount ts)

tdepth :: Tree a -> Int
tdepth (Node _ []) = 0
tdepth (Node _ ts) = 1 + maximum (map tdepth ts)

num :: (Int, Int)
num = (tcount tree, tdepth tree)
  where tree = gametree empty O

-- Ex2
randomElem :: [a] -> IO a
randomElem xs = do idx <- randomRIO (0,length xs - 1)
                   return (xs !! idx)

-- Ex3
quickestmove :: Tree Grid -> Grid -> Player -> Grid
quickestmove t _ p = minGrid
  where
    (minGrid, _) = minDepth [(g',tdepth n) | n@(Node (g', p') _) <- ts,
                                             p' == best]
    minDepth = minimumBy (\(_,a) (_,b) -> compare a b)
    tree = prune depth t
    Node (_, best) ts = alphabeta p tree

-- Ex4
-- a
askFirst :: IO Player
askFirst = do putStr "Do you want to go first? (y/n) "
              a <- getLine
              case a of
                "y" -> return O
                "n" -> return X
                _ -> do putStrLn "ERROR: Please answer y or n!"
                        askFirst

-- b
-- see wins

-- c
subtree :: Tree Grid -> Grid -> Tree Grid
subtree (Node _ ts) g = case find (\(Node g' _) -> g' == g) ts of
                          Just t -> t
                          Nothing -> undefined

-- d
type Interval = (Int, Int)

alphabeta :: Player -> Tree Grid -> Tree (Grid, Int)
alphabeta p t = alphabeta' p (-100, 100) t

alphabeta' :: Player -> Interval -> Tree Grid -> Tree (Grid, Int)
alphabeta' _ _ (Node g [])
  | wins O g = Node(g, -1) []
  | wins X g = Node(g, 1) []
  | otherwise = Node(g, 0) []

alphabeta' X (alpha, beta) (Node g ts) =
  Node (g, score) subtrees
  where
    score = maximum (map (\(Node (_, s) _) -> s) subtrees)
    subtrees = takeMax (-100) ts (alpha, beta)

alphabeta' O (alpha, beta) (Node g ts) =
  Node (g, score) subtrees
  where
    score = minimum (map (\(Node (_, s) _) -> s) subtrees)
    subtrees = takeMin 100 ts (alpha, beta)

takeMax :: Int -> [Tree Grid] -> Interval -> [Tree (Grid, Int)]
takeMax v (c:cs) (alpha, beta) =
  if alpha' >= beta || null cs then
    [node]
  else
    node : (takeMax value cs (alpha', beta))
  where
    node = alphabeta' O (alpha, beta) c
    Node(_, v') _ = node
    value = max v v'
    alpha' = max value alpha

takeMin :: Int -> [Tree Grid] -> Interval -> [Tree (Grid, Int)]
takeMin v (c:cs) (alpha, beta) =
  if alpha >= beta' || null cs then
    [node]
  else
    node : (takeMin value cs (alpha, beta'))
  where
    node = alphabeta' X (alpha, beta) c
    Node(_, v') _ = node
    value = min v v'
    beta' = min value beta
