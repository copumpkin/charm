{-# LANGUAGE ImplicitParams, TupleSections #-}
-- This should really be in a separate package
module Architecture.ARM.Decoder.Iteratee where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

data Iteratee i a = Done a | More (i -> (Iteratee i a, Maybe i))

instance Show a => Show (Iteratee i a) where
  showsPrec d (Done x) = showParen (d > 10) $ showString "Done " . showsPrec 11 x
  showsPrec d (More f) = showParen (d > 10) $ showString "More <function>"


caseIteratee :: (a -> b) -> ((i -> (Iteratee i a, Maybe i)) -> b) -> Iteratee i a -> b
caseIteratee d m (Done x) = d x
caseIteratee d m (More f) = m f

instance Functor (Iteratee i) where
  fmap = liftM

instance Applicative (Iteratee i) where
  pure = return
  (<*>) = ap

instance Monad (Iteratee i) where
    return = Done
    Done a >>= f = f a
    More k >>= f = More (docase . k)
     where
     docase (Done a, Just r) = case f a of
		   More k -> k r
		   i      -> (i, Just r)
     docase (i, s)  = (i >>= f, s)

fromIteratee :: Iteratee i a -> (i -> Iteratee i a)
fromIteratee (Done x) = const (Done x)
fromIteratee (More f) = fst . f


toIteratee :: (i -> Iteratee i a) -> Iteratee i a
toIteratee f = More ((, Nothing) . f)

steps :: Iteratee i a -> [i] -> (a, [i])
steps (Done y) xs = (y, xs)
steps (More f) (x:xs) = case f x of 
                          (g, Just r ) -> steps g (r:xs)
                          (g, Nothing) -> steps g xs


{-
instance Category Iteratee where
  id = More Done
  Done x . g = Done x
  More f . g = helper f g
    where helper f (Done x) = f x . Done x
          helper f (More g) = More (helper f . g)
-}