module Koan.Either where

import           Prelude          hiding (Either (..), isLeft, isRight, lefts, rights, either)
import           Data.Bifunctor

enrolled :: Bool
enrolled = True

data Either a b = Left a | Right b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

lefts :: [Either a b] -> [a]
-- lefts [] = []
-- lefts (Left x:xs) = x:(lefts xs)
-- lefts (Right x:xs) = (lefts xs)
-- lefts x = filter isLeft (_ x)
lefts xs = foldl go [] xs
  where go :: [a] -> Either a b -> [a]
        go c (Left l)   = c ++ [l]
        go c (Right _)  = c

rights :: [Either a b] -> [b]
rights xs = foldl go [] xs
  where go :: [b] -> Either a b -> [b]
        go c (Right r)   = c ++ [r]
        go c (Left _)  = c

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of
  Left l    -> f l
  Right r   -> g r

-- If you want a challenge, try to implement this using `foldr`
partition :: [Either a b] -> ([a], [b])
partition xs = foldr go ([], []) xs where
  go :: Either a b -> ([a], [b]) -> ([a], [b])
  go e (x, y) = case e of
    Left l    -> (l:x, y)
    Right r   -> (x, r:y)

mapEither :: (b -> c) -> Either a b -> Either a c
mapEither f e = case e of
  -- Right r   -> fmap f (Right r)
  Right r   -> Right (f r)
  Left l    -> Left l

bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapEither f g e = case e of
  Left l  -> Left (f l)
  Right r -> Right (g r)

applyEither :: Either a (b -> c) -> Either a b -> Either a c
applyEither (Right f) (Right x)  = Right (f x)
applyEither (Left l) _  = Left l
applyEither _ (Left l)  = Left l

bindEither :: (b -> Either a c) -> Either a b -> Either a c
bindEither f (Left l) = Left l
bindEither f (Right r) = f r

instance Functor (Either a) where
  fmap = mapEither

instance Bifunctor Either where
  bimap = bimapEither

instance Applicative (Either a) where
  pure = Right
  (<*>) = applyEither

instance Monad (Either a) where
  (>>=) = flip bindEither
