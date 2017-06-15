module Koan.Applicative where

import Prelude hiding (Applicative (..), Maybe (..))
-- import qualified Koan.Functor as KF
{-
## Pre-requisites
* Functor
-}

enrolled :: Bool
enrolled = True

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  -- (*>) c = (<*>) (id <$ c)
  (*>) c d = thing where
    -- thing = (id <$ c) <*> d
    thing = (flip const <$> c) <*> d

    -- thing = (<*>) (id <$ c) d

    -- thing = (<*>) (KF.fmap . const) d
  -- (*>) = (<*>) _f _g
  -- (*>) (g c) (g d) = (<*>) (g _) (g d)
  -- (*>) = error "TODO: implement (<*)"

  -- (*>) f g = g
  -- (*>) = (<*>)

  (<*) :: f a -> f b -> f a
  -- (<*) = error "TODO: implement (<*)"
  (<*) c d = (const <$> c) <*> d
