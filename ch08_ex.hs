main :: IO ()
main = do print ""

-- Ex1
data Nat = Zero | Succ Nat
  deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = add n (Succ m)

mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add m (mult n m)

-- Ex2

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node left node right) = case compare x node of
                                    LT -> occurs x left
                                    EQ -> True
                                    GT -> occurs x right

-- only needs one comparison in the worst case instead of two comparisons

-- Ex3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving (Show)

count_leaves :: Tree' a -> Int
count_leaves (Leaf' _) = 1
count_leaves (Node' left right) = count_leaves left + count_leaves right

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' left right) = abs (lcount - rcount) <= 1
                              && balanced left
                              && balanced right
  where lcount = count_leaves left
        rcount = count_leaves right

-- Ex4
halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
  where len = (length xs) `div` 2

balance :: [a] -> Tree' a
balance [] = error "can't create balanced tree from empty list"
balance [x] = Leaf' x
balance xs = Node' (balance left) (balance right)
  where (left, right) = halve xs

-- Ex5
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
  deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add left right) = g (folde f g left) (folde f g right)

-- Ex6
eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (\_ -> 1) (+) e

-- Ex7
data Maybe' a = Just' a | Nothing'

instance Eq a => Eq (Maybe' a) where
  Just' a == Just' b = a == b
  Nothing' == Nothing' = True
  _ == _ = False

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Eq a => Eq (List a) where
  Nil == Nil = True
  (Cons a as) == (Cons b bs) = a == b && as == bs
  _ == _ = False

-- Ex8

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          | Or Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

subst :: Prop -> [Subst]
subst p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

peval :: Subst -> Prop -> Bool
peval _ (Const b) = b
peval s (Var x) = find x s
peval s (Not x) = not (peval s x)
peval s (And p q) = peval s p && peval s q
peval s (Imply p q) = peval s p <= peval s q
peval s (Equiv p q) = peval s p == peval s q
peval s (Or p q) = peval s p || peval s q

isTaut :: Prop -> Bool
isTaut p = and [peval s p | s <- subst p]

-- Ex9
type Cont = [Op]
data Op = EVAL_ADD Expr | ADD Int | EVAL_MUL Expr | MUL Int

am_eval :: Expr -> Cont -> Int
am_eval (Val n) c = am_exec c n
am_eval (Add x y) c = am_eval x (EVAL_ADD y : c)
am_eval (Mul x y) c = am_eval x (EVAL_MUL y : c)

am_exec :: Cont -> Int -> Int
am_exec [] n = n
am_exec (EVAL_ADD y : c) n = am_eval y (ADD n : c)
am_exec (EVAL_MUL y : c) n = am_eval y (MUL n : c)
am_exec (ADD n : c) m = am_exec c (n+m)
am_exec (MUL n : c) m = am_exec c (n*m)

value :: Expr -> Int
value e = am_eval e []
