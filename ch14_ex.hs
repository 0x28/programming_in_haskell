import Data.Foldable

-- Ex1
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   mempty = (mempty, mempty)
--   (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- Ex2
-- instance Monoid b => Monoid (a -> b) where
--   mempty = \_ -> mempty
--   f `mappend` g = \x -> f x `mappend` g x

-- Ex3
newtype Option a = O (Maybe a)
  deriving Show

instance Foldable Option where
  fold (O (Just x)) = x
  fold (O Nothing) = mempty

  foldMap f (O (Just x)) = f x
  foldMap _ (O Nothing) = mempty

  foldr _ z (O Nothing) = z
  foldr f z (O (Just x)) = f x z

  foldl _ z (O Nothing) = z
  foldl f z (O (Just x)) = f z x

instance Functor Option where
  fmap f (O (Just x)) = O (Just (f x))
  fmap _ (O Nothing) = O Nothing

instance Traversable Option where
  traverse _ (O Nothing) = pure (O Nothing)
  -- (a -> f b) -> Option a -> f (Option b)
  traverse g (O (Just x)) = pure (\a -> O (Just a)) <*> g x

dec :: Int -> Maybe Int
dec x | x > 0 = Just (x - 1)
      | otherwise = Nothing

-- Ex4
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node l v r) = (foldMap f l) `mappend` (f v) `mappend` (foldMap f r)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse g (Node l v r) =
    pure Node <*> traverse g l <*> g v <*> traverse g r

-- Ex5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap f
  where f x | p x = [x]
            | otherwise = []

main :: IO ()
main = do return ()
