-- Ex1
-- ['a','b','c'] :: [Char]
-- ('a','b','c') :: (Char, Char, Char)
-- [(False,'0'),(True,'1')] :: [(Bool,Char)]
-- ([False,True],['0','1']) :: ([Bool],[Char])
-- (tail, init, reverse) :: ([a] -> [a], [b] -> [b], [c] -> [c])

-- Ex2
bools:: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f x = f x

-- Ex3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Ex5 Because they may not halt. If the functions halt you could check every
-- possible combination of arguments. Not realistic because for list and tuples
-- that set is infinite.
