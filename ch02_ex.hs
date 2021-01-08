-- Ex3
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

-- Ex4
last1 xs = head (reverse xs)

last2 xs = xs !! (length xs - 1)

-- Ex5
init1 xs = reverse (drop 1 (reverse xs))

init2 xs = take (length xs - 1) xs

init3 [x] = []
init3 (x:xs) = [x] ++ (init3 xs)
