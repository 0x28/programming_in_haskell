-- Ex1
-- redex: possible reduction in the next evaluation step

-- 1 + (2*3)
-- only one way
-- 2*3: innermost and outermost

-- (1+2) * (2+3)
-- one way because innermost and outermost are left to right
-- 1+2: innermost and outermost
-- 2+3: nether

-- fst (1+2, 2+3)
-- two ways because evaluation is left to right
-- 1+2: innermost
-- 2+3: nether
-- fst (1+2, 2+3): outermost

-- (\x -> 1 + x) (2*3)
-- call by value or call by name
-- 2*3: innermost
-- (\x -> 1 + x) (2*3): outermost

-- Ex2
-- outermost takes fewer steps because the second element of the tuple is never
-- evaluated:

-- outermost:
-- 1. fst (1+2,2+3)
-- 2. 1+2
-- 3. 3

-- innermost:
-- 1. fst (1+2,2+3)
-- 2. fst (3,2+3)
-- 3. fst (3,5)
-- 4. 3

-- Ex3
-- mult = \x -> (\y -> x * y)
-- 1. mult 3 4
-- 2. (\x (\y -> x * y)) 3 4
-- 3. (\y -> 3 * y) 4
-- 4. 3 * 4
-- 5. 12

-- Ex4
fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

-- Ex5
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

trepeat :: a -> Tree a
trepeat x = Node tree x tree
  where tree = trepeat x

ttake :: Int -> Tree a -> Tree a
ttake 0 _ = Leaf
ttake _ Leaf = Leaf
ttake n (Node left value right) = Node (ttake ln left) value (ttake rn right)
  where rn = (n - 1) `div` 2
        ln = (n - 1) `mod` 2 + rn

treplicate :: Int -> a -> Tree a
treplicate n = ttake n . trepeat

-- Ex6
sqroot :: Double -> Double
sqroot n | n < 0 = undefined
sqroot n = head [b | (a, b) <- zip approx (tail approx), abs (a-b) < dist]
  where approx = iterate (\a -> (a + n/a) / 2) 1.0
        dist = 0.00001

---
main :: IO ()
main = do return ()
