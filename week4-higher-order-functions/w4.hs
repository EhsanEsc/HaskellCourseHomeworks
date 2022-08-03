{-# LANGUAGE MultiWayIf #-}

-- Exercise 1
f1 :: [Integer] -> Integer
f1 = product . map (\x -> x-2) . filter (even)

findNext :: Integer -> Integer
findNext x 
    | even x = x `div` 2
    | otherwise = (3*x+1)

f2 :: Integer -> Integer
-- f2 x = sum $ filter (even) $ takeWhile (/=1) (iterate (\y -> ((1-(y `mod` 2)) * y `div` 2) + ((((y `mod` 2))) * (3*y+1))) x)
-- f2 x = sum $ filter (even) $ takeWhile (/=1) (iterate (\y -> if
--                                                         | even y -> y `div` 2 
--                                                         | otherwise -> (3*y+1) ) x)
f2 x = sum $ filter (even) $ takeWhile (/=1) (iterate (findNext) x)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- Exercise 2
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

addToTree :: Tree a -> a -> Tree a
addToTree Leaf x = Node 0 Leaf x Leaf
addToTree (Node ht Leaf a Leaf) x = (Node (ht+1) (Node 0 Leaf x Leaf) a Leaf)
addToTree (Node ht Leaf a r_tree) x = (Node ht (Node 0 Leaf x Leaf) a r_tree)
addToTree (Node ht l_tree a Leaf) x = (Node ht l_tree a (Node 0 Leaf x Leaf))
addToTree (Node ht (Node hl x1 a1 (x2)) a2 (Node hr ( x3) a3 ( x4))) x
    | hl==hr = (Node (ht+1) (Node hl ( x1) a1 ( x2)) a2 (addToTree (Node hr ( x3) a3 ( x4)) x))
    | otherwise = (Node (ht) (addToTree (Node hl ( x1) a1 ( x2)) x) a2 (Node hr ( x3) a3 ( x4)))

-- ! Not Working ! :(
-- Build Balanced Binary tree from list
-- foldTree :: [a] -> Tree a
-- foldTree ls = foldr (addToTree) Leaf ls

-- Exercise 3
xor :: Bool -> Bool -> Bool
xor b1 b2 = (/=) b1 b2
fxor :: [Bool] -> Bool
fxor ls = foldr (xor) False ls

map22 :: (a -> b) -> [a] -> [b]
map22 f [] = []
map22 f ls = foldr (\y ys -> (f y):ys) [] ls

-- Exercise 4

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = map (\x -> 2*x+2) $ filter (\(x, y) -> )

main = do
    print $ f1 [1, 4, 5, 6, 8]
    print $ f1 [1, 3]
    print $ f1 []
    print $ f1 [1, 2, 3, 4]
    print $ map f2 [1..10]
    print $ map fun2 [1..10]

    print $ (xor True False)
    print $ fxor [False, True, False, False, True] --False
    print $ fxor [False, True, False] --True
    -- print $ (foldTree "ABCDEFGHIJ")
                -- Node 3
                --  (Node 2
                --   (Node 0 Leaf ’F’ Leaf)
                --  ’I’
                --   (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
                --  ’J’
                --  (Node 2
                --   (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
                --  ’H’
                --   (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf)))
    print $ map22 f2 [1..10]
