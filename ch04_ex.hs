-- Ex1
halve :: [a] -> ([a], [a])
halve list = (take len list, drop len list)
             where len = div (length list) 2

-- Ex2
-- a
third1 :: [a] -> a
third1 list = head (tail (tail list))
-- b
third2 :: [a] -> a
third2 list = list !! 2
-- c
third3 :: [a] -> a
third3 (x1:x2:x3:_) = x3

-- Ex3
-- a
safetail1 :: [a] -> [a]
safetail1 list = if (null list) then [] else (tail list)
-- b
safetail2 :: [a] -> [a]
safetail2 list | null list = []
               | otherwise = tail list
-- c
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (x:xs) = xs

-- Ex4
-- (||) :: Bool -> Bool -> Bool
-- True  || True  = True
-- True  || False = True
-- False || True  = True
-- False || False = False

-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _     || _     = True

-- (||) :: Bool -> Bool -> Bool
-- True || _ = True
-- False || a = a

(||) :: Bool -> Bool -> Bool
a || b | a == b = b
       | otherwise = True

-- Ex5
-- (&&) :: Bool -> Bool -> Bool
-- a && b = if a then (if b then True else False) else False

-- Ex6
(&&) :: Bool -> Bool -> Bool
a && b = if a then b else False

-- Ex7
mult1 :: Int -> Int -> Int -> Int
mult1 x y z = x * y * z

mult2 :: Int -> (Int -> (Int -> Int))
mult2 = \x -> (\y -> (\z -> x * y * z))

-- Ex8
luhnDouble :: Int -> Int
luhnDouble n = if d > 9 then d - 9 else d
               where d = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0
