import System.IO

-- Ex1
putStr :: String -> IO ()
putStr str = sequence_ [putChar s | s <- str]

-- Ex2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do Main.putStr (show row)
                    Main.putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = helper 1 board
  where helper _ [] = return ()
        helper n (r:rs) = do putRow n r
                             helper (n + 1) rs

-- Ex3
putBoard2 :: Board -> IO ()
putBoard2 board = sequence_ [putRow r n | (r,n) <- zip [1..] board]

-- Ex4
read_int :: IO Int
read_int = do num <- getLine
              return (read num)

adder' :: Int -> Int -> IO ()
adder' sum 0 = do Prelude.putStr "The total is "
                  putStrLn (show sum)
adder' sum rest = do num <- read_int
                     adder' (num + sum) (rest - 1)

adder :: IO ()
adder = do Prelude.putStr "How many numbers? "
           hFlush stdout
           rest <- read_int
           adder' 0 rest

-- Ex5
adder2 :: IO ()
adder2 = do Prelude.putStr "How many numbers? "
            hFlush stdout
            num <- read_int
            actions <- sequence (replicate num read_int)
            Prelude.putStr "The total is "
            Prelude.putStrLn (show (sum actions))

-- Ex6
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine' :: String -> IO String
readLine' str = do next <- getCh
                   case next of
                     '\n' -> return str
                     '\DEL' -> if null str then readLine' ""
                               else do putChar '\b'
                                       readLine' (tail str)
                     _ -> readLine' (next:str)

readLine :: IO String
readLine = do line <- readLine' ""
              return (reverse line)

main :: IO ()
main = do line <- readLine
          putStrLn line
          Main.putStr "size: "
          putStrLn (show (length line))
