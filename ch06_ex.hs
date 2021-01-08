-- Ex1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac(n - 1)

-- Ex2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

-- Ex3
(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ n | n > 0 = x * (x Main.^ (n - 1))

-- 2 ^ 3
-- 2 * (2 ^ 2)
-- 2 * (2 * (2 ^ 1))
-- 2 * (2 * (2 * (2 ^ 0)))
-- 2 * (2 * (2 * (1)))
-- 8

-- Ex4
euclid :: Int -> Int -> Int
euclid a b | a > b = euclid (a-b) b
           | a < b = euclid (b-a) a
           | otherwise = a

-- Ex5
-- length [] = 0
-- length (x:xs) = 1 + length xs

-- length [1,2,3]
-- 1 + length [2,3]
-- 1 + 1 + length [3]
-- 1 + 1 + 1 + length []
-- 1 + 1 + 1 + 0
-- 3

-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_:xs) = drop (n-1) xs

-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]

-- init [_] = []
-- init (x:xs) = x : init xs

-- init [1,2,3]
-- 1 : init [2,3]
-- 1 : 2 : init [3]
-- 1 : 2 : []
-- [1,2]

-- Ex6
-- a.
and :: [Bool] -> Bool
and [] = True
and (True:xs) = Main.and xs
and (False:xs) = False

-- b.
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss

-- c.
replicate :: Int -> a -> [a]
replicate 0 e = []
replicate n e | n > 0 = e : Main.replicate (n-1) e

-- d.
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n | n > 0 = xs Main.!! (n-1)

-- e.
elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) | e == x = True
              | otherwise = Main.elem e xs

-- Ex7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Ex8
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x], [])
halve (x1:x2:xs) = (x1 : left, x2: right)
  where (left, right) = halve xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = halve xs

-- Ex9
-- a.
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs

-- b.
take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) | n > 0 = x : Main.take (n-1) xs

-- c.
last :: [a] -> a
last [x] = x
last (_:xs) = Main.last xs
