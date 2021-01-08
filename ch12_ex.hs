-- Ex1

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node left v right) = Node left' (f v) right'
    where left' = fmap f left
          right' = fmap f right

-- Ex2
newtype Fun a b = F (a -> b)

instance Functor (Fun a) where
  -- (a -> b) -> f a -> f b
  -- (a -> b) -> (x -> a) -> (x -> b)
  fmap f (F g) = F (f . g)

-- Ex3
instance Applicative (Fun a) where
  -- x -> f x
  -- x -> (a -> x)
  pure x = F (\_ -> x)
  -- f (a -> b) -> f a -> f b
  -- (x -> a -> b) -> (x -> a) -> (x -> b)
  (F f) <*> (F g) = F (\x -> f x (g x))

-- Ex4
newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  pure x = Z (repeat x)
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- Ex5
-- 1. pure id <*> x = x
-- id            :: a -> a
-- pure          :: a -> f a
-- pure id       :: f (a -> a)
-- pure id <*> x :: f a

-- 2. pure (g x) = pure g <*> pure x
-- g                 :: a -> b
-- x                 :: a
-- pure (g x)        :: f b
-- pure g            :: f (a -> b)
-- pure x            :: f a
-- <*>               :: f (a -> b) -> f a -> f b
-- pure g <*> pure x :: f b

-- 3. x <*> pure y = pure (\g -> g y) <*> x
-- x                      :: f (a -> b)
-- pure y                 :: f a
-- x <*> pure y           :: f b
-- pure (\g -> g y)       :: f ((a -> b) -> b)
-- pure (\g -> g y) <*> x :: f b

-- 4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- x                          :: f (b -> c)
-- y                          :: f (a -> b)
-- z                          :: f a
-- (y <*> z)                  :: f b
-- x <*> (y <*> z)            :: f c
-- pure (.)                   :: f ((b -> c) -> (a -> b) -> (a -> c))
-- pure (.) <*> x             :: f ((a -> b) -> (a -> c))
-- pure (.) <*> x <*> y       :: f (a -> c)
-- pure (.) <*> x <*> y <*> z :: f c

-- Ex6
instance Monad (Fun a) where
  return = pure
  -- m x -> (x -> m y) -> m y
  -- (a -> x) -> (x -> (a -> y)) -> (a -> y)
  (F g) >>= f = F (\a -> let (F f') = f (g a) in (f' a))

-- Ex7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Var v) = Var (f v)
  fmap _ (Val n) = Val n
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure n                      = Var n
  _           <*> (Val n)     = (Val n)
  (Val n)     <*> _           = (Val n)
  (Var f)     <*> (Var v)     = Var (f v)
  (Var f)     <*> (Add l r)   = fmap f (Add l r)
  (Add l r)   <*> (Var v)     = Add (l <*> (Var v)) (r <*> (Var v))
  (Add l1 r1) <*> (Add l2 r2) = Add (l1 <*> l2) (r1 <*> r2)

instance Monad Expr where
  return = pure
  -- Expr a -> (a -> Expr b) -> Expr b
  (Var e) >>= f = f e
  (Val n) >>= _ = Val n
  (Add l r) >>= f = Add (l >>= f) (r >>= f)

-- the following function is the lookup table for variables in expressions
table :: String -> Expr Int
table "var1" = Val 100
table "var2" = Val 200
table _      = undefined

e1 = Add (Var "var1") (Add (Var "var2") (Val 2))

-- the bind for Expr substitutes the variables for their values
e2 = e1 >>= table
-- or
e3 = do e <- e1
        table e
-- result: Add (Val 100) (Add (Val 200) (Val 2))

-- Ex8
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap g st = do s <- st
                 return (g s)

instance Applicative ST where
  pure x = S (\s -> (x,s))
  stf <*> stx = do f <- stf
                   x <- stx
                   return (f x)

instance Monad ST where
  st >>= f = S(\s ->
                 let (x, s') = app st s in app (f x) s')

main :: IO ()
main = do return ()
