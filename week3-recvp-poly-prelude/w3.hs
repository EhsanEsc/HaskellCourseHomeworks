
-- Input List -> Counter -> Remaining -> Result List
f :: [a] -> Int -> Int -> [a]
f [] _ _ = []
f (x:xs) ct rm
    | (ct == (rm+1)) = x : (f xs ct 0)
    | otherwise = f xs ct (rm + 1)

ff :: [a] -> Int -> [a]
ff ls n = f ls n 0

-- Input list -> Counter -> Remaining -> Result List of Lists
g :: [a] -> Int -> Int -> [[a]]
g ls ct rm
    | (ct == (rm-1)) = []
    | otherwise = ff ls rm : g ls ct (rm+1)

skips :: Eq a => [a] -> [[a]]
skips [] = []
skips ls = g ls (length ls) 1

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:ls)
    | (y>x) && (y>z) = y : localMaxima (y:z:ls)
    | otherwise = localMaxima (y:z:ls)
localMaxima _ = []

-- Exercise 3
count :: [Integer] -> Integer -> Integer
count [] _ = 0
count (x:xs) c
    | (x == c) = 1 + count xs c
    | otherwise = count xs c

-- Input List -> current -> result
fcount :: [Integer] -> Integer -> [Integer]
fcount _ 10 = []
fcount ls x = (count ls x) : fcount ls (x+1)

-- Input List -> height -> result
createRow :: [Integer] -> Integer -> String
createRow [] _= ""
createRow (x:xs) height 
    | x>=height = '*' : createRow xs height
    | otherwise = " " ++ createRow xs height

-- Input List -> MaxHeight -> result
createAllRows :: [Integer] -> Integer -> String
createAllRows _ 0 = ""
createAllRows ls h = createRow ls h ++ "\n" ++ createAllRows ls (h-1)

getMax :: [Integer] -> Integer
getMax [] = 0
getMax (x:xs) 
    | (x > getMax xs) = x
    | otherwise = getMax xs

histogram :: [Integer] -> String
histogram ls = createAllRows (fcount ls 0) (getMax $ (fcount ls 0)) ++ "==========\n0123456789\n"

main = do
    print $ "Hello World"
    print $ (f [1, 2, 3, 4, 5, 6, 7, 8] 2 0)
    print $ (skips "ABCD")
    print $ (skips "ABCD" == ["ABCD", "BD", "C", "D"])
    print $ (skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"])
    print $ (skips [1] == [[1]])
    print $ (skips [True,False] == [[True,False], [False]])
    -- print $ ((skips []) == []) -- Compile Error xD
    
    print $ localMaxima [2,9,5,6,1] == [9,6]
    print $ localMaxima [2,3,4,1,5] == [4]
    print $ localMaxima [1,2,3,4,5] == []
    putStr(histogram [1,1,1,5])
    putStr(histogram [1,4,5,4,6,6,3,4,2,4,9])
    putStr(histogram [3,5])
    -- print $ fcount [1,1,1,5] 0
    -- print $ getMax $ fcount [1,1,1,5] 0
    -- print $ createAllRows (fcount [3, 5] 0) (getMax $ fcount [3, 5] 0)
