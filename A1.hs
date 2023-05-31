{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module A1 where

import Data.Bits

--q1
pentanacci :: (Eq a, Num a, Num p) => a -> p
pentanacci 1 = 0
pentanacci 2 = 0
pentanacci 3 = 0
pentanacci 4 = 0
pentanacci 5 = 1
pentanacci x = pentanacci (x-1) + pentanacci (x-2) + pentanacci (x-3) + pentanacci (x-4) + pentanacci (x-5)



--q2
solve :: Int -> Int -> [Int]
solve n x = let q = quot n x
                r = rem n x
                firstpart = take (x-r) $ repeat q
                secondpart = take r $ repeat (q+1)
            in firstpart ++ secondpart


--q3
isPrefix :: String -> String -> Bool
isPrefix x y
    | length x <= length y = and [a == b | (a,b) <- zip x y]
    | otherwise = False

changeSpace :: String -> String
changeSpace (x:xs)
    | notElem '-' (x:xs) = x:xs
    | x == '-' = ' ':changeSpace xs
    |otherwise = x:changeSpace xs

lookupName :: [String] -> String -> [String]
lookupName [] _ = []
lookupName y "" = [changeSpace a| a <- y]
lookupName (y:ys) n
    | isPrefix n y = changeSpace y: lookupName ys n
    | otherwise = lookupName ys n


--q4
g :: Bool -> Bool -> Bool
g True True = True
g True False = True
g False True = False
g False False = False

sortp :: [a] -> (a -> a -> Bool) -> [a]
sortp (y:ys) f
    | length (y:ys) == 0 = []
    | length (y:ys) == 1 = [y]
    | length [b |b <- ys,f y b || ((not (f y b)) && (not (f b y)))] == length ys = y: sortp ys f
    | otherwise = sortp (ys ++ [y]) f


--q5
foldlp ::(b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlp p f n (y:ys)
    | p $ f n y = foldlp p f (f n y) ys
    | otherwise = n


--q6
-- group in pairs
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

--check ascending 
isEvensAscending :: [Int] -> Bool
isEvensAscending xs = and [ x <= y | (x,y) <- pairs (filter even xs)]

--check descending
isOddsDescending :: [Int] -> Bool
isOddsDescending xs = and [ x >= y | (x,y) <- pairs (filter odd xs)]

--main
isTwinPaired :: [Int] -> Bool
isTwinPaired xs = isEvensAscending xs && isOddsDescending xs


--q7
--read
readI :: String -> Int
readI = read

--rules
stack :: [String] -> String -> [String]
stack (x:y:zs) "+" = show (readI x + readI y):zs
stack (x:y:zs) "-" = show (readI y - readI x):zs
stack (x:y:zs) "*" = show (readI x * readI y):zs
stack (x:y:zs) "/" = show (div (readI y) (readI x)):zs
stack (x:y:zs) "&" = show ((.&.) (readI x) (readI y)):zs
stack (x:y:zs) "|" = show ((.|.) (readI x) (readI y)):zs
stack (x:xs) "inc" = show ((readI x)+1):xs
stack (x:xs) "dec" = show ((readI x)-1):xs
stack (x:y:zs) "changeSpace" = []
stack (x:xs) "dup" = x:(x:xs)
stack ns num = num : ns

--main
solveRPN :: String -> Int
solveRPN n = readI (head (foldl stack ["0"] (words n)))