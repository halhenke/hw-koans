module Koan.Eq where

import           Prelude hiding (elem, filter)

enrolled :: Bool
enrolled = True

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt f [] = []
filterInt f (x:xs)
  | f x         = x : filterInt f xs
  | otherwise   = filterInt f xs

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar f "" = ""
filterChar f (x:xs)
  | f x         = x : filterChar f xs
  | otherwise   = filterChar f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
  | f x         = x : filter f xs
  | otherwise   = filter f xs

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt f [] = False
elemInt f (x:xs)
  | f == x      = True
  | otherwise   = elemInt f xs

elem :: Eq a => a -> [a] -> Bool
elem f [] = False
elem f (x:xs)
  | f == x      = True
  | otherwise   = elem f xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:(nub (filter (x /=) xs))

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:as) (b:bs)
  | a == b = isPrefixOf as bs
  | otherwise = False
