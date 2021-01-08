-- Ex6

data Op = Add | Sub | Mul | Div | Exp
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e = "(" ++ show e ++ ")"

type Result = (Expr, Int)

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y > 1 && x `mod` y == 0
valid' Exp x y = y > 1 && x > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid' o x y]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine' lx ry]

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


choices :: [a] -> [[a]]
choices xs = [c | s <- subs xs, c <- perms s]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]

qsort :: (a -> a -> Ordering) -> [a] -> [a]
qsort _ [] = []
qsort o (x:xs) = qsort o smaller ++ [x] ++ qsort o larger
      where
        smaller = [a | a <- xs, o a x == LT]
        larger  = [b | b <- xs, o b x /= LT]

nearest_solutions :: [Int] -> Int -> [(Expr, Int)]
nearest_solutions ns n =
  (take 10 . (qsort cmp))
  [(e, m) | ns' <- choices ns, (e, m) <- results ns']
  where
    cmp (e1, v1) (e2, v2) = case compare (dist v1) (dist v2) of
                              EQ -> compare (length (values e1))
                                            (length (values e2))
                              c  -> c
    dist a = (abs (n - a))

main :: IO ()
main = do print (nearest_solutions [1,3,7,10,25,50] 831)
