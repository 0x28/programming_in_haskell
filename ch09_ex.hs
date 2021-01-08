data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l        <- exprs ls,
                 r        <- exprs rs,
                 e        <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- Ex1
-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs
choices :: [a] -> [[a]]
choices xs = [c | s <- subs xs, c <- perms s]

-- Ex2
remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x:xs) | e == x = xs
                | otherwise = x : remove e xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (y:ys) xs = elem y xs && isChoice ys (remove y xs)

-- Ex3

-- A split of any list would always return one tuple with the complete list and
-- an empty list. This would prevent the reduction in the recursive "exprs".
-- Therefore "exprs" would never terminate.

-- Ex4
possible_exprs = concat (map exprs (choices [1,3,7,10,25,50]))
valid_exprs = (filter (/= []) . (map eval)) possible_exprs

-- main :: IO ()
-- main = do print (length possible_exprs)
--           print (length valid_exprs)

-- Ex5
valid_int :: Op -> Int -> Int -> Bool
valid_int Add _ _ = True
valid_int Sub _ _ = True
valid_int Mul _ _ = True
valid_int Div x y = y /= 0 && x `mod` y == 0

eval_int :: Expr -> [Int]
eval_int (Val n) = [n | n > 0]
eval_int (App o l r) = [apply o x y | x <- eval_int l,
                                      y <- eval_int r,
                                      valid_int o x y]

valid_int_exprs = (filter (/= []) . (map eval_int)) possible_exprs

main :: IO ()
main = do print (length valid_int_exprs)

-- Ex6
-- see ./ch9_ex6.hs
