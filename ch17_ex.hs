import Control.Applicative

-- Ex 1
data Expr = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr
  deriving (Show)

type Stack = [Maybe Int]

eval :: Expr -> Maybe Int
eval (Val n) = Just n
-- eval (Add x y) = case eval x of
--                    Just n -> case eval y of
--                      Just m -> Just (n + m)
--                      Nothing -> Nothing
--                    Nothing -> Nothing
-- rewrite:
eval (Add x y) = pure (+) <*> eval x <*> eval y
eval Throw = Nothing
-- eval (Catch x h) = case eval x of
--                      Just n -> Just n
--                      Nothing -> eval h
-- rewrite:
eval (Catch x h) = (eval x) <|> (eval h)

-- exec (comp e) s = eval e : s             -- Equation E1
-- exec (comp' e c) s = exec c (eval e : s) -- Equation E2

-- case Val n:

-- exec (comp' (Val n) c) s
-- = { use E2 }
-- exec c (eval (Val n) : s)
-- = { apply eval }
-- exec c (Just n : s)
-- = { define exec (PUSH n c) s = exec c (Just n : s) }
-- = { unapply exec }
-- exec (PUSH n c) s

-- therefore: comp' (Val n) c = PUSH n c

-- case Throw:

-- exec (comp' Throw c) s
-- = { use E2 }
-- exec c (eval Throw : s)
-- = { apply eval }
-- exec c (Nothing : s)
-- = { define exec (THROW c) s = exec c (Nothing : s) }
-- = { unapply exec }
-- exec (THROW c) s

-- therefore: comp' Throw c = THROW c

-- case (Add x y):

-- exec (comp' (Add x y) c) s
-- = { use E2 }
-- exec c (eval (Add x y) : s)
-- = { rewrite eval (Add x y) using the applicative operator }
-- = { apply eval }
-- exec c ((pure (+) <*> eval x <*> eval y) : s)

---- To use the induction hypothesis we need to satisfy the following equation:
-- exec c' (eval y : eval x : s) = exec c ((pure (+) <*> eval x <*> eval y) : s)
-- = { generalize }
-- exec c' (m : n : s) = exec c ((pure (+) <*> n <*> m) : s)
----

-- = { define exec (ADD c) (m : n : s) = exec c ((pure (+) <*> n <*> m) : s) }
-- = { unapply exec }
-- exec (ADD c) (eval y : eval x : s)
-- = { induction hypothesis E2 on y }
-- exec (comp' y (ADD c)) (eval x : s)
-- = { induction hypothesis E2 on x }
-- exec (comp' x (comp' y (ADD c))) s

-- therefore comp' (Add x y) c = comp' x (comp' y (ADD c))

-- case (Catch x h):

-- exec (comp' (Catch x h) c) s
-- = { use E2 }
-- exec c (eval (Catch x h) : s)
-- = { rewrite eval (Catch x h) using <|> }
-- = { apply eval }
-- exec c (((eval x) <|> (eval h)) : s)

---
-- exec c' (eval h : eval x : s) = exec c (((eval x) <|> (eval h)) : s)
-- = { generalize }
-- exec c' (x' : h' : s) = exec c ((x' <|> h') : s)
---

-- = { define exec (CATCH c) (x : h : s) = exec c ((x <|> h) : s) }
-- = { unapply exec }
-- exec (CATCH c) (eval x : eval h : s)
-- = { induction hypothesis on x }
-- exec (comp' x (CATCH c)) (eval h : s)
-- = { induction hypothesis on h }
-- exec (comp' h (comp' x (CATCH c))) s

-- therefore: comp' (Catch x h) c = comp' h (comp' x (CATCH c))

-- comp:

-- exec (comp e) s
-- = { using E1 }
-- eval e : s
-- = { define exec HALT s = s}
-- = { unapply exec }
-- exec HALT (eval e : s)
-- = { using E2 }
-- exec (comp' e HALT) s

-- therefore comp e = comp' e HALT

-------------
-- Results --
-------------

data Code = HALT
  | ADD Code
  | PUSH Int Code
  | THROW Code
  | CATCH Code
  deriving (Show)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' Throw c = THROW c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' h (comp' x (CATCH c))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (Just n : s)
exec (THROW c) s = exec c (Nothing : s)
exec (ADD c) (m : n : s) = exec c ((pure (+) <*> n <*> m) : s)
exec (CATCH c) (x : h : s) = exec c ((x <|> h) : s)

printExpr :: Expr -> IO ()
printExpr e = do putStr (show e)
                 putStr " => "
                 print (exec (comp e) [])

main :: IO ()
main =
  do printExpr e1
     printExpr e2
     printExpr e3
     printExpr e4
     printExpr e5
       where e1 = Add (Val 10) (Val 20)
             e2 = Throw
             e3 = Add Throw (Val 42)
             e4 = Add (Catch
                        (Add Throw (Val 100))
                        (Val 0))
                      (Val 20)
             e5 = (Catch Throw Throw)
