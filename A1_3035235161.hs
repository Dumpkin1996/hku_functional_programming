import Data.Char
import Test.QuickCheck

--Problem 1
--function COMBINATIONS takes integer k and n, and returns the number of k-combinations selected from n elements.
combinations :: Int -> Int -> Int
combinations 0 n = 1
combinations k n = if (k==n) then 1 else combinations k (n-1) + combinations (k-1) (n-1)


--Problem 2
--function POSITION returns the index of the first element which equals to the query value in the given list.
position :: Eq a => a -> [a] -> Int
position a [] = -1
position a (x : xs) = if (a == x) then 0 
    else if (position a xs == -1) then -1 else 1 + position a xs


--Problem 3
--function INTTOCOLUMN returns the index of the  rst element which equals to the query value in the given list.
intToColumn :: Int -> String
intToColumn 0 = ""
intToColumn x = intToColumn (x `div` 26) ++ [chr (x `mod` 26 + 64)]


--Problem 4
--function COLUMNTOINT returns the corresponding integer as the column number for a given column title.
columnToInt :: String -> Int
columnToInt "" = 0
columnToInt s =  (26 ^ l) * (ord (head s) - 64) + columnToInt (tail s)
    where x = ord (head s) - 64
          l = length (tail s)


--Problem 5
--PROCOLUMNNUMBER is a property for QuickCheck to check INTTOCOLUMN and COLUMNTOINT
propColumnNumber :: Int -> Property
propColumnNumber x = (x > 0) ==> columnToInt (intToColumn x) == x


--Problem 6
--function ISPREFIX takes in two lists and checks if the first list is the prefix of the second
isprefix :: (Eq a) => [a] -> [a] -> Bool
isprefix [] _ =  True
isprefix _ [] =  False
isprefix (x:xs) (y:ys) = x == y && isprefix xs ys

--function LOOKUPPHONENUMBER takes a phonebook as a list of strings, and a pre x of phone number as a single string,
--return all numbers that start with the pre x.
lookupPhoneNumber :: [String] -> String -> [String]
lookupPhoneNumber xs p = [s | s <- processed, isprefix p s]
    where processed = [filter (\x -> x /= ' ' && x /= '-') s| s <- xs]


--Problem 7
--function PERMUTATIONS returns all permutations of the input list.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = concat [addx y | y <- permutations xs]
  where addx = \y -> [ fst (splitAt i y) ++ [x] ++ snd (splitAt i y) | i <- [0..(length y)] ]
 