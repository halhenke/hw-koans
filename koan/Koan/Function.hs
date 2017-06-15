module Koan.Function where

import           Prelude          hiding ((.), ($))

import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K

enrolled :: Bool
enrolled = True

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

-- Hint: You're already implemented this.
mapFunction :: (a -> b) -> (r -> a) -> r -> b
mapFunction = error "TODO: Implement mapFunction"

applyFunction :: (r -> a -> b) -> (r -> a) -> r -> b
applyFunction = error "TODO: implement applyFunction"

bindFunction :: (a -> r -> b) -> (r -> a) -> r -> b
bindFunction = error "TODO: implement bindFunction"

instance K.Functor ((->) r) where
  fmap = error "TODO: Implement fmap for (->)"

instance K.Applicative ((->) r) where
  pure = error "TODO: Implement Applicative pure for (->)"
  (<*>) = error "TODO: Implement Applicative (<*>) for (->)"

instance K.Monad ((->) r) where
  (>>=) = error "TODO: Implement Monad (>>=) for (->)"
