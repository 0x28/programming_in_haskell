-- Ex1
double :: Num a => a -> a
double x = x + x
-- double (double 2)
-- double (2 + 2)
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8

-- Ex2
mysum [] = 0
mysum (n:ns) = n + mysum ns
-- sum [x]
-- x + sum []
-- x + 0
-- x

-- Ex3
myproduct [] = 1
myproduct (n:ns) = n * myproduct ns
-- product [2,3,4]
-- 2 * product [3,4]
-- 2 * 3 * product [4]
-- 2 * 3 * 4 * product []
-- 2 * 3 * 4 * 1
-- 24

-- Ex4
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
      where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

rsort [] = []
rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller
      where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- Ex5
badsort [] = []
badsort (x:xs) = badsort smaller ++ [x] ++ badsort larger
      where
        smaller = [a | a <- xs, a < x]
        larger  = [b | b <- xs, b > x]

-- badsort [2,2,3,1,1]
-- badsort [1,1] ++ [2] ++ badsort [3]
-- ([] ++ [1] ++ []) ++ 2 ++ [3]
-- [1,2,3]
-- duplicates get removed
