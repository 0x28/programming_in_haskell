import Data.Char

-- Ex1
a :: (a -> b) -> (a -> Bool) -> [a] -> [b]
a f p xs = [f x | x <- xs, p x]

a' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
a' f p = map f . (filter p)

-- Ex2
-- a
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

-- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs

-- Ex3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if p x then x : y else y) []

-- Ex4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Ex5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \a b -> f (a,b)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(a, b) -> f a b

-- Ex6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f s = unfold (\_ -> False) f f s

-- Ex7
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode :: [Bit] -> String
decode = map (chr . bin2int . check_parity) . chop9

encode :: String -> [Bit]
encode = concat . map (add_parity . make8 . int2bin . ord)

channel = id

transmit :: String -> String
transmit = decode . channel . encode

add_parity :: [Bit] -> [Bit]
add_parity bits | sum bits `mod` 2 == 0 = 0 : bits
                | otherwise = 1 : bits

check_parity :: [Bit] -> [Bit]
check_parity bits | sum bits `mod` 2 == 0 = tail bits
                  | otherwise = error "parity check failed!"

-- Ex8
faulty_transmit :: String -> String
faulty_transmit = decode . tail . encode

-- Ex9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- Ex10
luhnDouble :: Int -> Int
luhnDouble n = if d > 9 then d - 9 else d
               where d = n * 2

luhn :: [Int] -> Bool
luhn nums = sum (altMap (id) (luhnDouble) (reverse nums)) `mod` 10 == 0
