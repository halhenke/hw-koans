module Koan.Function where

import           Prelude          hiding ((.), ($))

import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K

enrolled :: Bool
enrolled = False

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = error "TODO: Implement (.)"

flip :: (a -> b -> c) -> b -> a -> c
flip = error "TODO: Implement flip"

<<<<<<< HEAD
($) :: (a -> b) -> a -> b
($) = error "TODO: Implement ($)"

instance K.Functor ((->) a) where
  fmap = error "TODO: Implement fmap for (->)"

instance K.Applicative ((->) a) where
  pure = error "TODO: Implement Applicative pure for (->)"
  (<*>) = error "TODO: Implement Applicative (<*>) for (->)"

instance K.Monad ((->) a) where
=======
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
>>>>>>> 04e2858ee94a449b2b51491e77e1b6b7a50ddba9
  (>>=) = error "TODO: Implement Monad (>>=) for (->)"