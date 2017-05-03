module Koan.Ord where

import           Prelude hiding (max, maximum, min, minimum)

enrolled :: Bool
enrolled = True

-- | Get the greater of two values
max :: Ord a => a -> a -> a
max a b
  | a > b     = a 
  | otherwise = b

-- | Get the lesser of two values
min :: Ord a => a -> a -> a
-- min a b = max b a
min a b
  | a < b     = a 
  | otherwise = b

-- | Get the greatest element of the list.  The list must be finite and non-empty.
maximum :: Ord a => [a] -> a
maximum (x:xs) = let
  helper a (b:bs) = helper (max a b) bs
  helper a [] = a
  in
    helper x xs

-- | Get the least element of the list.  The list must be finite and non-empty.
minimum :: Ord a => [a] -> a
minimum (x:xs) = let
  helper a (b:bs) = helper (min a b) bs
  helper a [] = a
  in
    helper x xs

-- -- | The 'sort' function implements a stable sorting algorithm.
-- sort :: Ord a => [a] -> [a]
-- sort [] = []
-- sort (x:xs) = let
--   helper a [] = a
--   -- helper a (b:bs) | ((min a b) == a) = a : (helper b bs)
--   -- helper a (b:bs) | (min a b) == b = b : (helper a bs)
--   -- helper a (b:bs) = case (min a b) of
--   helper a (b:bs) = if (a < b) 
--     then a : sort bs
--     else b : sort bs
--   -- helper a (b:bs) = case (a < b) of
--   --   True ->   a : (helper b bs)
--   --   False ->  b : (helper a bs)
--     -- a -> a : _ --(helper b bs)
--     -- b -> b : (helper a bs)
--   in
--     helper x xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = (sort $ filter (x >) xs) ++ [x] ++ (sort $ filter (x <) xs) 
-- sort a = foldMap minimum a
-- sort x:xs = foldl minimum a
-- sort [] = []
-- sort (x:xs) 
--   | (x <= (minimum xs))
--   then x : sort xs
--   else sort (xs ++ [x])
-- sort (x:xs) = let
--     helper :: Ord a => [a] -> [a] -> [a] -> [a]
--     helper a [] _ = a
--     helper a (b:bs) c = helper (min ) bs 
--   in helper [] (x:xs) []
    -- True ->   a : (helper b bs)
    -- False ->  b : (helper a bs)  
  -- | helper x (minimum xs) == x = x : sort xs
  -- | _                       = 

-- sort (x:xs) = let
--   helper :: Ord a => a -> [a] -> [a]
--   helper a [] = [a]
--   helper a (b:bs) = case (a < b) of
--     True ->   a : (helper b bs)
--     False ->  b : (helper a bs)
--   in
--     helper x xs

-- | The 'insert' function takes an element and a list and inserts the
-- element into the list at the first position where it is less
-- than or equal to the next element.  In particular, if the list
-- is sorted before the call, the result will also be sorted.
-- It is a special case of 'insertBy', which allows the programmer to
-- supply their own comparison function.
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = case (a <= x) of
  True -> a : (x:xs)
  _ -> x : insert a xs
