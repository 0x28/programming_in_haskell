import Data.Char

-- Ex1
e1 = sum [x^2 | x <- [1..100]]

-- Ex2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

-- Ex3
square :: Int -> [(Int, Int)]
square n = [(a,b) | (a,b) <- grid n n, a /= b]

-- Ex4
myreplicate :: Int -> a -> [a]
myreplicate n e = [e | _ <- [1..n]]

-- Ex5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Ex6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum (factors x) - x == x]

-- Ex7
expected7 :: [(Int, Int)]
expected7 = [(x,y) | x <- [1,2], y <- [3,4]]

myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

e7 :: [(Int, Int)]
e7 = myconcat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- Ex8
find :: Eq a => a -> [(a,b)] -> [b]
find k m = [v | (k', v) <- m, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- Ex9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a * b | (a,b) <- zip xs ys]

-- Ex10

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'

int2letLower :: Int -> Char
int2letLower n = chr (ord 'a' + n)

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2letLower ((let2int c + n) `mod` 26)
          | isUpper c = int2letUpper ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
