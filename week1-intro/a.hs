-- Exercise 1
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
myRev :: [a] -> [a]

myRev [] = []
myRev (x:xs) = myRev xs ++ [x]

toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : (toDigitsRev ((n `div` 10)))
toDigits n = myRev $ toDigitsRev n

-- Exercise 2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:(y:ls)) = x : y*2 : doubleEveryOtherRev ls
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls = myRev $ doubleEveryOtherRev $ myRev ls

-- Exercise 3
sumDigitsOne :: Integer -> Integer
sumDigitsOne 0 = 0
sumDigitsOne n = sumDigitsOne (n `div` 10) + (n `mod` 10)
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ls) = sumDigitsOne x + sumDigits ls

-- Exercise 4
validate :: Integer -> Bool
validate x = ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- Count, From, Helper, To
hanoi 1 t1 t2 t3 = [(t1, t3)]
hanoi n t1 t2 t3 = hanoi (n-1) t1 t3 t2 ++ [(t1, t3)] ++ hanoi (n-1) t2 t1 t3

main = do
    print $ toDigits 123
    print $ toDigits 13579
    print $ doubleEveryOther [8, 7, 6, 5]
    print $ doubleEveryOther [1, 2, 3]
    print $ sumDigitsOne 12345
    print $ sumDigits [16,7,12,5]
    print $ toDigits 4012888888881881
    print $ doubleEveryOther $ toDigits 4012888888881881
    print $ sumDigits $ doubleEveryOther $ toDigits 4012888888881881
    print $ validate 4012888888881881 -- True
    print $ validate 4012888888881882 -- False
    print $ hanoi 2 "a" "b" "c"
    print $ hanoi 3 "a" "b" "c"