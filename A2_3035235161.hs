{-

COMP3258: Functional Programming

Assignment 2

Deadline: 23:59, March 21, 2017 (HKT)

-}

import Data.Char (ord, chr, isLetter)

{-

DO NOT ADD OTHER IMPORTS!

Unless specified, xoou should not use functions from other libraries apart from
the standard librarxo Prelude

-}

-- | Problem 1

enumIntFromTo :: Int -> Int -> [Int]
enumIntFromTo x xo = if x > xo then []
                       else [x] ++ enumIntFromTo (x + 1) xo

enumIntFromThenTo :: Int -> Int -> Int -> [Int]
enumIntFromThenTo x xd xo
    | d >= 0 = if x > xo then [] else [x] ++ enumIntFromThenTo (x + d) (xd + d) xo
    | d < 0 = if x < xo then [] else [x] ++ enumIntFromThenTo (x + d) (xd + d) xo
        where d = xd - x

-- | Problem 2
-- xoou can assume that messages onlxo use uppercase letters (A-Z) and white spaces.

caesar :: Int -> String -> String
caesar a "" = ""
caesar a (x : xs) 
    | x == ' ' = [x] ++ caesar a xs
    | xo + a > 90 = [chr (xo + a - 26)] ++ caesar a xs 
    | otherwise = [chr (xo + a)] ++ caesar a xs
        where xo = ord x

-- | Problem 3
unCaesar :: Int -> String -> String
unCaesar a "" = ""
unCaesar a (x : xs) 
    | x == ' ' = [x] ++ unCaesar a xs
    | xo - a < 65 = [chr (xo - a + 26)] ++ unCaesar a xs 
    | otherwise = [chr (xo - a)] ++ unCaesar a xs
        where xo = ord x

-- | Problem 4
vigenere :: String -> String -> String
vigenere _ "" = ""
vigenere "" s = s
vigenere (k : ks) (x : xs)
    | x == ' ' = [x] ++ vigenere (k : ks) xs
    | xo + ko > 90 = [chr (xo + ko - 26)] ++ vigenere (ks ++ [k]) xs
    | otherwise = [chr (xo + ko)] ++ vigenere (ks ++ [k]) xs
       where xo = ord x
             ko = ord k - 65

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- | Problem 5

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node t1 y t2)
    | x < y = Node (insert x t1) y t2
    | x >= y = Node t1 y (insert x t2)

-- | Problem 6

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node t1 x t2) = [x] ++ preorder t1 ++ preorder t2

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node t1 x t2) = inorder t1 ++ [x] ++ inorder t2

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node t1 x t2) = postorder t1 ++ postorder t2 ++ [x]

-- | Problem 7

toTree :: Ord a => [a] -> Tree a
toTree [] = Leaf
toTree (x : xs) = insert x (toTree xs)

sort :: Ord a => [a] -> [a]
sort = inorder . toTree

data ListZipper a =
  ListZipper [a]
             a
             [a]
  deriving (Eq, Show)

-- | Problem 8

toList :: ListZipper a -> [a]
toList (ListZipper xs a ys) = (reverse xs) ++ [a] ++ ys

-- | Problem 9

moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (ListZipper [] _ _) = Nothing
moveLeft (ListZipper (x : xs) a ys) = Just $ ListZipper xs x ([a] ++ ys) 

moveRight :: ListZipper a -> Maybe (ListZipper a)
moveRight (ListZipper _ _ []) = Nothing
moveRight (ListZipper xs a (y : ys)) = Just $ ListZipper ([a] ++ xs) y ys
