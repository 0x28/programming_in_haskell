-- Ex1
-- Prove: add n (Succ m) = Succ (add n m)

-- base case:
-- add Zero (Succ m)
-- = { apply add }
-- Succ m
-- = { unapply add on m }
-- Succ (add Zero m)

-- inductive case:
-- add (Succ n) (Succ m)
-- = { apply add }
-- Succ (add n (Succ m))
-- = { induction hypothesis }
-- Succ (Succ (add n m))
-- = { unapply inner add }
-- Succ (add (Succ n) m)
-- q.e.d.

-- Ex2
-- Prove: add n m = add m n

-- base case:
-- add Zero m
-- = { apply add }
-- m
-- = { using add n Zero = n }
-- add m Zero

-- inductive case:
-- add (Succ n) m
-- = { apply add }
-- Succ (add n m)
-- = { using induction hypothesis on inner add }
-- Succ (add m n)
-- = { using Ex1 }
-- add m (Succ n)
-- q.e.d.

-- Ex3
-- all p [] = True
-- all p (x:xs) = p x && all p xs

-- Prove: all (== x) (replicate n x) = True

-- base case:
-- all (== x) (replicate 0 x)
-- = { apply replicate }
-- all (== x) []
-- = { apply all }
-- True

-- inductive case:
-- all (== x) (replicate (n + 1) x)
-- = { apply replicate }
-- all (== x) (x : replicate n x)
-- = { apply all }
-- (== x) x && all (== x) (replicate n x)
-- = { simplify }
-- all (== x) (replicate n x)
-- = { induction hypothesis }
-- True
-- q.e.d.

-- Ex4
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

-- Prove: xs ++ [] = xs

-- base case:
-- [] ++ []
-- = { apply ++ }
-- []

-- inductive case:
-- (x:xs) ++ []
-- = { apply ++ }
-- x : (xs ++ [])
-- = { using induction hypothesis }
-- x : xs
-- q.e.d.

-- Prove: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- base case:
-- [] ++ (ys ++ zs)
-- = { apply first ++ }
-- ys ++ zs
-- = { add parentheses }
-- (ys) ++ zs
-- = { unapply ++ on ys }
-- ([] ++ ys) ++ zs

-- inductive case:
-- (x:xs) ++ (ys ++ zs)
-- = { apply outer ++ }
-- x : (xs ++ (ys ++ zs))
-- = { induction hypothesis }
-- x : ((xs ++ ys) ++ zs)
-- = { unapply ++ }
-- (x : (xs ++ ys)) ++ zs
-- = { unapply ++ }
-- ((x:xs) ++ ys) ++ zs
-- q.e.d.

-- Ex5
-- Prove: take n xs ++ drop n xs = xs

-- base case 1:
-- take 0 xs ++ drop 0 xs
-- = { apply take }
-- [] ++ drop 0 xs
-- = { apply drop }
-- [] ++ xs
-- = { apply ++ }
-- xs

-- base case 2:
-- take n [] ++ drop n []
-- = { apply take n }
-- [] ++ drop n []
-- = { apply drop }
-- [] ++ []
-- = { apply ++ }
-- []

-- inductive case:
-- take (n+1) (x:xs) ++ drop (n+1) (x:xs)
-- = { apply take }
-- (x : take n xs) ++ drop (n+1) (x:xs)
-- = { apply drop }
-- (x : take n xs) ++ (drop n xs)
-- = { unapply ++ }
-- [x] ++ take n xs ++ drop n xs
-- = { induction hypothesis }
-- [x] ++ xs
-- = { apply ++ }
-- x : (xs ++ [])
-- = { apply ++ }
-- x : xs
-- q.e.d.

-- Ex6
data Tree = Leaf Int | Node Tree Tree

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = 1 + countNodes l + countNodes r

countLeafs :: Tree -> Int
countLeafs (Leaf _) = 1
countLeafs (Node l r) = countLeafs l + countLeafs r

-- Prove: countLeafs t - countNodes t = 1

-- base case:
-- countLeafs (Leaf n) - countNodes (Leaf n)
-- = { apply countLeafs }
-- 1 - countNodes (Leaf n)
-- = { apply countNodes }
-- 1 - 0
-- =
-- 1

-- inductive case:
-- countLeafs (Node l r) - countNodes (Node l r)
-- = { apply countLeafs }
-- countLeafs l + countLeafs r - countNodes (Node l r)
-- = { apply countNodes }
-- countLeafs l + countLeafs r - (1 + countNodes l + countNodes r)
-- = { simplify }
-- countLeafs l - countNodes l + countLeafs r - countNodes r - 1
-- = { induction hypothesis }
-- 1 + 1 - 1
-- = { simplify }
-- 1
-- q.e.d.

-- Ex7
-- fmap id = id
-- fmap (g . h) = fmap g . fmap h

-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap g (Just x) = Just (g x)

-- Prove: fmap id m = id m
-- where m is a Maybe

-- case Nothing:
-- fmap id Nothing
-- = { apply fmap }
-- Nothing
-- = { unapply id }
-- id Nothing

-- case Just x:
-- fmap id (Just x)
-- = { apply fmap }
-- Just (id x)
-- = { apply id }
-- Just x
-- = { unapply id }
-- id (Just x)
-- q.e.d.

-- Prove: fmap (g . h) m = (fmap g . fmap h) m

-- case Nothing:
-- fmap (g . h) Nothing
-- = { apply fmap }
-- Nothing
-- = { unapply fmap }
-- fmap g Nothing
-- = { unapply fmap }
-- fmap g (fmap h Nothing)
-- = { introduce lambda }
-- (\m -> fmap g (fmap h m)) Nothing
-- = { unapply . }
-- (fmap g . fmap h) Nothing

-- case Just x:
-- fmap (g . h) (Just x)
-- = { apply fmap }
-- Just ((g . h) x)
-- = { apply . }
-- Just (g (h x))
-- = { unapply fmap }
-- fmap g (Just (h x))
-- = { unapply inner fmap }
-- fmap g (fmap h (Just x))
-- = { introduce lambda }
-- (\m -> fmap g (fmap h m)) (Just x)
-- = { unapply . }
-- (fmap g . fmap h) (Just x)
-- q.e.d.

-- Ex8
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- instance Functor Tree where
-- fmap g (Leaf x)   = Leaf (g x)
-- fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- Prove: fmap id t = id t

-- base case:
-- fmap id (Leaf x)
-- = { apply fmap }
-- Leaf (id x)
-- = { apply id }
-- Leaf x
-- = { unapply id }
-- id (Leaf x)

-- inductive case:
-- fmap id (Node l r)
-- = { apply id }
-- Node (fmap id l) (fmap id r)
-- = { induction hypothesis }
-- Node (id l) (id r)
-- = { apply id }
-- Node l r
-- = { unapply id }
-- id (Node l r)
-- q.e.d.

-- Prove: fmap (g . h) t = (fmap g . fmap h) t

-- base case:
-- fmap (g . h) (Leaf x)
-- = { apply . }
-- fmap (\t -> g (h t)) (Leaf x)
-- = { apply fmap }
-- Leaf (g (h x))
-- = { unapply fmap }
-- fmap g (Leaf (h x))
-- = { unapply inner fmap }
-- fmap g (fmap h (Leaf x))
-- = { unapply . }
-- (fmap g . fmap h) (Leaf x)

-- inductive case:
-- fmap (g . h) (Node l r)
-- = { apply fmap }
-- Node (fmap (g . h) l) (fmap (g . h) r)
-- = { induction hypothesis }
-- Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
-- = { apply . }
-- Node ((\t -> fmap g (fmap h t)) l) ((\t -> fmap g (fmap h t)) r)
-- = { apply lambda }
-- Node (fmap g (fmap h l)) (fmap g (fmap h r))
-- = { unapply fmap }
-- fmap g (Node (fmap h l) (fmap h r))
-- = { unapply fmap }
-- fmap g (fmap h (Node l r))
-- = { unapply . }
-- (fmap g . fmap h) (Node l r)
-- q.e.d.

-- Ex9
-- pure id <*> x   = x                            -- Identity
-- pure (g x)      = pure g <*> pure x            -- Homomorphism
-- x <*> pure y    = pure (\g -> g y) <*> x       -- Interchange
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z -- Composition

-- instance Applicative Maybe where
--   pure            = Just
--   Nothing <*> _   = Nothing
--   (Just g) <*> mx = fmap g mx

-- Prove: pure id <*> x = x

-- case Nothing:
-- pure id <*> Nothing
-- (Just id) <*> Nothing
-- fmap id Nothing
-- Nothing

-- case Just x:
-- pure id <*> (Just x)
-- (Just id) <*> (Just x)
-- fmap id (Just x)
-- Just (id x)
-- Just x
-- q.e.d.

-- Prove: pure (g x) = pure g <*> pure x

-- case Nothing:
-- pure (g Nothing)
-- Just (g Nothing)
-- = { unapply fmap }
-- fmap g (Just Nothing)
-- = { unapply <*> }
-- (Just g) <*> (Just Nothing)
-- = { unapply pure }
-- pure g <*> pure Nothing

-- case Just x:
-- pure (g (Just x))
-- Just (g (Just x))
-- = { unapply fmap }
-- fmap g (Just (Just x))
-- = { unapply <*> }
-- (Just g) <*> (Just (Just x))
-- = { unapply pure }
-- pure g <*> pure (Just x)
-- q.e.d.

-- Prove: x <*> pure y = pure (\g -> g y) <*> x

-- case Nothing:
-- Nothing <*> pure y
-- Nothing
-- = { unapply fmap }
-- fmap (\g -> g y) Nothing
-- = { unapply <*> }
-- Just (\g -> g y) <*> Nothing
-- pure (\g -> g y) <*> Nothing

-- case Just f:
-- (Just f) <*> pure y
-- (Just f) <*> (Just y)
-- = { apply <*> }
-- fmap f (Just y)
-- = { apply fmap }
-- Just (f y)
-- = { add lambda }
-- Just ((\g -> g y) f)
-- = { unapply fmap }
-- fmap (\g -> g y) (Just f)
-- = { unapply <*> }
-- (Just (\g -> g y)) <*> (Just f)
-- = { unapply pure }
-- pure (\g -> g y) <*> (Just f)
-- q.e.d.

-- Prove: x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
----------------------------------------------------
-- first Prove: x <*> Nothing
-- case Nothing:
-- Nothing <*> Nothing
-- Nothing

-- case Just x:
-- (Just x) <*> Nothing
-- fmap x Nothing
-- Nothing
-- q.e.d.
----------------------------------------------------

-- case x is Nothing:
-- Nothing <*> (y <*> z)
-- = { apply <*> }
-- Nothing

-- (pure (.) <*> Nothing <*> y) <*> z
-- (fmap (.) (Nothing <*> y)) <*> z
-- (fmap (.) Nothing) <*> z
-- Nothing <*> z
-- Nothing

-- case y is Nothing:
-- x <*> (Nothing <*> z)
-- x <*> Nothing
-- = { use x <*> Nothing = Nothing }
-- Nothing

-- (pure (.) <*> x <*> Nothing) <*> z
-- (fmap (.) (x <*> Nothing)) <*> z
-- (fmap (.) Nothing) <*> z
-- Nothing <*> z
-- Nothing

-- case z is Nothing:
-- x <*> (y <*> Nothing)
-- = { use y <*> Nothing = Nothing }
-- x <*> Nothing
-- = { use x <*> Nothing = Nothing }
-- Nothing

-- (pure (.) <*> x <*> y) <*> Nothing
-- = { use x <*> Nothing = Nothing }
-- Nothing

-- case x, y, and z are Just:
-- (Just f) <*> ((Just g) <*> (Just h))
-- = { apply <*> }
-- fmap f ((Just g) <*> (Just h))
-- = { apply <*> }
-- fmap f (fmap g (Just h))
-- = { apply fmap }
-- fmap f (Just (g h))
-- = { apply fmap }
-- Just (f (g h))
-- = { unapply . }
-- Just ((f . g) h)
-- = { unapply fmap }
-- fmap (f . g) (Just h)
-- = { unapply <*> }
-- (Just (f . g)) <*> (Just h)
-- = { rewrite . }
-- (Just ((.) f g)) <*> (Just h)
-- = { unapply fmap }
-- (fmap ((.) f) (Just g)) <*> (Just h)
-- = { unapply <*> }
-- ((Just ((.) f)) <*> (Just g)) <*> (Just h)
-- = { unapply fmap }
-- ((fmap (.) (Just f)) <*> (Just g)) <*> (Just h)
-- = { unapply <*> }
-- (Just (.) <*> (Just f) <*> (Just g)) <*> (Just h)
-- = { unapply pure }
-- (pure (.) <*> (Just f) <*> (Just g)) <*> (Just h)
-- q.e.d.

-- Ex10
-- return x >>= f   = f x
-- mx >>= return    = mx
-- (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

-- instance Monad [] where
--   return x = pure x = [x]
--   xs >>= f = [y | x <- xs, y <- f x]

-- Prove: return x >>= f = f x

-- return x >>= f
-- = { apply return }
-- [x] >>= f
-- = { apply >>= }
-- [y | a <- [x], y <- f a]
-- = { substitute x }
-- [y | y <- f x]
-- = { f x creates a list fs }
-- [y | y <- fs ]
-- = { simplify }
-- fs
-- f x
-- q.e.d.

-- Prove: mx >>= return = mx

-- mx >>= return
-- = { apply >>= }
-- [y | x <- mx, y <- return x]
-- = { apply return }
-- [y | x <- mx, y <- [x]]
-- = { simplify }
-- [y | y <- mx]
-- = { simplify }
-- mx
-- q.e.d.

-- Prove: (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

-- (mx >>= f) >>= g
-- = { apply >>= }
-- [y | x <- mx, y <- f x] >>= g
-- = { apply >>= }
-- [a | b <- [y | x <- mx, y <- f x], a <- g b]
-- = { flatten list comprehension }
-- [a | x <- mx, b <- f x, a <- g b]
-- = { add nested list comprehension }
-- [a | x <- mx, a <- [b | c <- f x, b <- g c]]
-- = { rename variables }
-- [y | x <- mx, y <- [a | b <- f x, a <- g b]]
-- = { unapply >>= }
-- [y | x <- mx, y <- (f x >>= g)]
-- = { introduce lambda }
-- [y | x <- mx, y <- (\x -> (f x >>= g)) x]
-- = { unapply >>= }
-- mx >>= (\x -> (f x >>= g))
-- q.e.d.

-- Ex11
-- comp' e c = comp e ++ c

-- base case:
-- comp' (Val n) c
-- = { specification }
-- comp (Val n) ++ c
-- = { apply comp }
-- [Push n] ++ c
-- = { apply ++ }
-- Push n : c

-- inductive case:
-- comp' (Add x y) c
-- = { specification }
-- comp (Add x y) ++ c
-- = { apply comp }
-- comp x ++ comp y ++ [Add] ++ c
-- = { apply ++ }
-- comp x ++ comp y ++ (Add : c)
-- = { induction hypothesis }
-- comp x ++ comp' y (Add : c)
-- = { induction hypothesis }
-- comp' x (comp' y (Add : c))

main :: IO ()
main = do return ()
