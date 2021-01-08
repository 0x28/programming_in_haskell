import Control.Applicative
import Data.Char
import System.IO
import System.Exit

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x, xs)])

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])

  pg <*> px = P (\inp -> case parse pg inp of
                  [] -> []
                  [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\_ -> [])
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- letter
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]


cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display :: String -> IO ()
display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

beep :: IO ()
beep = putStr "\BEL"

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
             else
               do err (length xs)
                  calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = do goto (1,14)
          exitSuccess

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])]  -> calc (show n)
            [(_, out)] -> do err (length out)
                             calc xs
            []         -> do beep
                             calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do hSetBuffering stdout NoBuffering
         hSetBuffering stdin NoBuffering -- no ncurses :) nice
         cls
         showbox
         clear

-- Ex1
comment :: Parser ()
comment = do _ <- symbol "--"
             _ <- many (sat (/= '\n'))
             _ <-  (string "\n" <|> pure [])
             return ()

-- Ex2
-- 2+3+4
--
-- a)
--             expr
--            / | \
--           /  |  \
--          /   |   \
--        expr  +   term
--       / | \        |
--      /  |  \       |
--     /   |   \      |
--   term  +   term  factor
--    |         |      |
--    |         |      |
--    |         |      |
-- factor     factor  nat
--    |         |      |
--    |         |      |
--    |         |      |
--   nat       nat     4
--    |         |
--    |         |
--    |         |
--    2         3
--
--
-- b)
--              expr
--             / | \
--            /  |  \
--           /   |   \
--        term       expr
--         /        / | \
--        /        /  |  \
--       /        /   |   \
--   factor     term  +   term
--     /         |         |
--    /          |         |
--   /           |         |
-- nat        factor     factor
--  |            |         |
--  |            |         |
--  |            |         |
--  2           nat       nat
--               |         |
--               |         |
--               |         |
--               3         4


-- Ex3
-- 2+3
--      expr
--     / | \
--    /  |  \
--  term +  expr
--   |        |
--   |        |
-- factor    term
--   |        |
--   |        |
--  nat      factor
--   |        |
--   |        |
--   2       nat
--            |
--            |
--            3
--

-- 2*3*4
--          expr
--           |
--           |
--          term
--         / | \
--        /  |  \
--   factor  *   term
--    /          / | \
--   /          /  |  \
-- nat      factor *  term
--  |         |         |
--  |         |         |
--  2        nat       factor
--            |          |
--            |          |
--            3         nat
--                       |
--                       |
--                       4

-- (2*3)+4
--              expr
--             / | \
--            /  |  \
--         term  +   expr
--          /         |
--         /          |
--     factor       term
--      /| \          |
--     / |  \         |
--    ( expr )      factor
--      /| \          |
--     / |  \         |
--  term +  expr     nat
--   |       |        |
--   |       |        |
-- factor   term      4
--   |       |
--   |       |
--  nat    factor
--   |       |
--   |       |
--   2      nat
--           |
--           |
--           3

-- Ex4
-- Parse the number 1 using the third grammar:
-- expr -> expr + expr -> fail
-- expr -> term -> term * term -> fail
-- expr -> term -> factor -> ( expr ) -> fail
-- expr -> term -> factor -> nat -> 1

-- Parse the number 1 using the final grammar:
-- expr -> term -> factor -> ( expr ) -> fail
-- expr -> term -> factor -> nat -> 1

-- The third grammar causes the parser to retreat multiple times and use the
-- alternative rule. The final grammar is right recursive and therefore consumes
-- simpler rules faster.

-- Ex5

data Expr = Number Int | Add Expr Expr | Mul Expr Expr
  deriving Show

expr5 :: Parser Expr
expr5 = do t <- term5
           do symbol "+"
              e <- expr5
              return (Add t e)
            <|> return t

term5 :: Parser Expr
term5 = do f <- factor5
           do symbol "*"
              t <- term5
              return (Mul f t)
            <|> return f

factor5 :: Parser Expr
factor5 = do symbol "("
             e <- expr5
             symbol ")"
             return e
           <|> do n <- natural
                  return (Number n)

-- Ex6
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do f <- expt
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  e <- term
                  return (f `div` e)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

-- Ex7
-- ...
-- term ::= expt (* term | / term | ɛ)
-- expt ::= factor (^ expt | ɛ)
-- ...

expt :: Parser Int
expt = do f <- factor
          do symbol "^"
             e <- expt
             return (f ^ e)
         <|> factor

-- Ex8
-- a.

-- sub ::= (sub - nat) | nat
-- nat ::= 0 | 1 | 2 ...

-- b.
sub :: Parser Int
sub = do s <- sub
         symbol "-"
         n <- nat
         return (s - n)
        <|> nat

-- c.
-- It's left recursive. Therefore it will recursive forever.

-- d.
sub2 :: Parser Int
sub2 = do n <- nat
          nums <- many (do symbol "-"
                           s <- nat
                           return s)
          return (foldl (-) n nums)

-- Ex9
err :: Int -> IO ()
err c = do writeat (0, length box + 1) ("Error in column " ++ show c)


main :: IO ()
main = do run
