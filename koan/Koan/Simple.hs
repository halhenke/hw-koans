-- These are some simple problems to get your feet wet writing Haskell.
-- They assume you've been introduced to at least the basic syntax of the
-- language.

-- See here if you'd like a basic tour of the syntax:
-- https://github.com/haskell-works/hw-examples/blob/master/src/Examples.hs

-- Do as much as you think is useful. It doesn't matter if you don't complete
-- all of these, because they're all in the standard library (though
-- occasionally with slightly different type signatures.)

module Koan.Simple where

-- This is just hiding some of the standard library, as we're implemeting a lot
-- of it as an exercise.
import           Prelude hiding (all, any, const, curry, drop, dropWhile, elem, filter, flip, foldl, foldr, id, iterate, length, map, max, maximum, min, minimum, repeat, reverse, take, takeWhile, uncurry, zipWith, (!!), ($), (++), (.))

enrolled :: Bool
enrolled = True

-- There is only a single possible definition of the first two functions.
-- Try to work out what they need to do based on their type signature alone.
id :: a -> a
id a = a

const :: a -> b -> a
const a b = a

-- This is function composition, i.e. (f . g)(x) == f(g(x))
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) a b x = a (b x) 

-- This flips the argument order of a function, i.e.
-- f(a,b) == (flip f)(b,a)
flip :: (a -> b -> c) -> b -> a -> c
flip a b c = a c b

-- This is function application (i.e. calling a function), but with a lower
-- precedence than normal
($) :: (a -> b) -> a -> b
($) a x = a x

-- Precedence definition for ($). You don't need to do anything here.
infixr 0 $

--------------------------------------------------------------------------------
-- LISTS
--------------------------------------------------------------------------------

-- These are some simple operations on lists.

-- Return the length of a list
length :: [a] -> Int
length (x:xs) = 1 + length xs
length [] = 0

-- This returns the element at a specific index in a list (if it exists),
-- or nothing otherwise.
-- You'll probably need to use pattern matching.
(!!) :: [a] -> Int -> Maybe a
-- (!!) = error "TODO: Implement (!!)"
(!!) (x:xs) b
  | b > 0     = (!!) xs (b - 1)
  | otherwise = Just x
(!!) [] b = Nothing



-- ASIDE: In the standard library, (!!) has the type `[a] -> Int -> a`
-- meaning it fails (crashes the program) if the index you request does not
-- exist. Returning `Maybe a` is the safer way to implement it.

-- This concatenates two lists together.
-- Again, you'll probably need to use pattern matching.
(++) :: [a] -> [a] -> [a]
(++) (x:xs) b = (:) x ((++) xs b)
(++) [] b = b

-- Reverse a list.
-- Note that while you can get a "correct" solution using (++), it will be very
-- inefficient. Try to implement it without that. You may need a helper
-- function or a sub function.
reverse :: [a] -> [a]
-- reverse (x:xs) = (reverse xs) : [x]
reverse [] = [] 
reverse x = let
  helper [] y = y
  helper (y:ys) z = helper ys (y:z)
  in helper x [] 

--------------------------------------------------------------------------------
-- Infinite lists
--------------------------------------------------------------------------------

-- These functions should happily work on infinite lists.

-- repeat the input forever.
repeat :: a -> [a]
repeat a = a:repeat a
-- repeat [] = [[]]
-- repeat (x:xs) = [x] ++ (repeat xs)

-- Return a list of the result of repeatedly applying f to x
-- i.e iterate f x = [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]
-- iterate f [] = [] 
-- iterate f (x:xs) = (f x):(iterate f _xs)
iterate f x = x : iterate f (f x)

-- Returns the first `n` elements of a list.
-- If there are less than `n` elements, just return all of them.
take :: Int -> [a] -> [a]
take n [] = []
take n (x:xs)
  | n > 0     = x : take (n - 1) xs
  | otherwise = []

-- Similar to the above, but drop the first `n` elements and return the rest.
-- If there are less than `n` elements, return an empty list.
drop :: Int -> [a] -> [a]
drop n [] = []
drop n (x:xs)
  | n <= 0      = (x:xs)
  | otherwise   = drop (n - 1) xs
-- drop n xs     | n <= 0 =  xs
-- drop _ []              =  []
-- drop n (_:xs)          =  drop (n-1) xs

-- Take as long as the elements in the list satisfy the given predicate.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = [] 
takeWhile f (x:xs) 
  | f x       = x : takeWhile f xs
  | otherwise = []

-- You get the idea.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = [] 
dropWhile f (x:xs) 
  | f x       = dropWhile f xs
  | otherwise = x:xs
-- dropWhile _ []          =  []
-- dropWhile p xs@(x:xs')
--             | p x       =  dropWhile p xs'
--             | otherwise =  xs

--------------------------------------------------------------------------------
-- Higher order functions on lists
--------------------------------------------------------------------------------

-- These are the classic map, filter, and fold (a.k.a reduce) for lists.

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = case (f x) of
  True -> x : filter f xs
  _ -> filter f xs

-- The 'l' in `foldl` indicates that it is left-associative.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i [] = i
foldl f i (x:xs) = foldl f (f i x) xs 

-- Similarly, the 'r' indicates `foldr` is right associative.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i [] = i
foldr f i (x:xs) = f x (foldr f i xs)

-- Functions which tell you whether any or all of the elements of a list
-- satisfy a given predicate.
any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs)
  | f x       = True
  | otherwise = any f xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs)
  | not (f x)       = False
  | otherwise     = all f xs

-- This should take a binary (i.e. two argument) function, and two lists, and
-- "zip" the elements of the list together pairwise using that function.
-- If one input is shorter than the other, it should discard the excess
-- elements of the longer list.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f a [] = []
zipWith f [] b = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--------------------------------------------------------------------------------
-- Currying
--------------------------------------------------------------------------------

-- Turn an uncurried function (i.e. one that takes a tuple as its argument)
-- and curry it (i.e. make it take its arguments one-by-one)
curry :: ((a, b) -> c) -> a -> b -> c
curry = error "TODO: Implement curry"

-- The inverse of the above.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = error "TODO: Implement uncurry"

--------------------------------------------------------------------------------
-- Functions require Equality
--------------------------------------------------------------------------------

-- Determines if an element is in a list
elem :: Eq a => a -> [a] -> Bool
elem = error "TODO: Implement elem"

--------------------------------------------------------------------------------
-- Functions require Ordering
--------------------------------------------------------------------------------

-- Take to guess what these should do!

max :: Ord a => a -> a -> a
max = error "TODO: Implement max"

min :: Ord a => a -> a -> a
min = error "TODO: Implement min"

-- Note: These can fail if the list is empty.
-- We'll alow that in thise case. You can use the `error` function to return
-- an error.
maximum :: Ord a => [a] -> a
maximum = error "TODO: Implement maximum"

minimum :: Ord a => [a] -> a
minimum = error "TODO: Implement minimum"

--------------------------------------------------------------------------------
-- Miscellaneous Exercises
--------------------------------------------------------------------------------

-- Define a list of all fibonacci numbers.
-- (Hint: Try to use the `zipWith` function defined above)
fibonacci :: [Int]
fibonacci = error "TODO: Implement fibonacci"
