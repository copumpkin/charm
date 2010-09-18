{-# LANGUAGE ImplicitParams, TupleSections #-}
-- This should really be in a separate package
module Architecture.ARM.Decoder.Iteratee where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

data Iteratee i a = Done a | More (i -> Iteratee i a)

instance Show a => Show (Iteratee i a) where
  showsPrec d (Done x) = showParen (d > 10) $ showString "Done " . showsPrec 11 x
  showsPrec d (More f) = showParen (d > 10) $ showString "More <function>"


caseIteratee :: (a -> b) -> ((i -> Iteratee i a) -> b) -> Iteratee i a -> b
caseIteratee d m (Done x) = d x
caseIteratee d m (More f) = m f

instance Functor (Iteratee i) where
  fmap f (Done x) = Done $ f x
  fmap f (More g) = More $ fmap f . g

instance Applicative (Iteratee i) where
  pure = Done
  Done f <*> Done x = Done $ f x
  Done f <*> More g = More $ fmap f . g
  More f <*> q      = More $ (<*> q) . f

instance Monad (Iteratee i) where
  return = Done
  Done x >>= g = g x
  More f >>= g = More $ g <=< f

fromIteratee :: Iteratee i a -> i -> Iteratee i a
fromIteratee (Done x) = pure (Done x)
fromIteratee (More f) = f

toIteratee :: (i -> Iteratee i a) -> Iteratee i a
toIteratee f = More f

steps :: Iteratee i a -> [i] -> (a, [i])
steps (Done y) xs = (y, xs)
steps (More f) (x:xs) = steps (f x) xs


instance Category Iteratee where
  id = More Done
  Done x . g = Done x
  More f . g = helper f g
    where helper f (Done x) = f x . Done x
          helper f (More g) = More (helper f . g)

