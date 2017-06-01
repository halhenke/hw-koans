module Koan.List where

import           Prelude hiding (concat, head, init, last, reverse, tail, (++))
import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K


enrolled :: Bool
enrolled = True

-- Example:
--   head [1, 2, 3, 4] = 1
head :: [a] -> a
head (x:xs) = x

-- Example:
--   tail [1, 2, 3, 4] = [2, 3, 4]
tail :: [a] -> [a]
tail (x:xs) = xs

-- Example:
--   last [1, 2, 3, 4] = 4
last :: [a] -> a
last [a] = a
last (x:xs) = last xs

-- revHelper :: a -> [a] -> [a]
-- revhelper

-- Example:
--   reverse [1, 2, 3, 4] = [4, 3, 2, 1]
reverse :: [a] -> [a]
reverse [] = []
reverse x = let
  helper [] b = b
  helper (a:as) b = helper as (a:b)
  in helper x []

-- Example:
--   [1, 2] ++ [3, 4] = [1, 2, 3, 4]
(++) :: [a] -> [a] -> [a]
(++) [] y = y
(++) (x:xs) y = x:(xs ++ y)
-- (++) x (b:bs) = (x:b) ++ bs

-- Example:
--   concat [[1, 2], [3, 4]] = [1, 2, 3, 4]
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- Example:
--   tails [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = [x:xs] ++ (tails xs)

-- Example:
--   mapList show [1, 2, 3, 4] = ["1", "2", "3", "4"]
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = (f x) : (mapList f xs)

-- Example:
--   filterList even [1, 2, 3, 4] = [2, 4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs) = case (f x) of
  False     -> filterList f xs
  otherwise -> x : (filterList f xs)

-- Example:
--   foldlList (+) 0 [1, 2, 3] = 6
foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList f x [] = x
foldlList f x (y:ys) = foldlList f z ys where
  z = f x y

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList f x [] = x
foldrList f x (y:ys) = foldrList f z ys where
  z = f y x

-- Note that those are square brackets, not round brackets.
applyList :: [a -> b] -> [a] -> [b]
applyList [] y = []
applyList f [] = []
applyList (f:fs) (y:ys) = (f y) : (applyList fs ys)

bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (x:xs) = (f x) ++ (bindList f xs)

instance K.Functor [] where
  fmap = mapList

instance K.Applicative [] where
  pure a = [a]
  (<*>) = applyList

instance K.Monad [] where
  (>>=) = flip bindList
