{-# LANGUAGE GADTs #-}
module Architecture.ARM.Instructions.UAL.Semantics where
  
import Architecture.ARM.Instructions.UAL

-- This should be put somewhere central
data Expr a where
  Const  :: a -> Expr a
  (:+)   :: Expr a -> Expr a -> Expr a
  (:-)   :: Expr a -> Expr a -> Expr a
  (:*)   :: Expr a -> Expr a -> Expr a
  (:/)   :: Expr a -> Expr a -> Expr a
  (:<<)  :: Expr a -> Expr a -> Expr a
  (:>>)  :: Expr a -> Expr a -> Expr a
  (:<<<) :: Expr a -> Expr a -> Expr a
  (:>>>) :: Expr a -> Expr a -> Expr a
  (:&)   :: Expr a -> Expr a -> Expr a
  (:|)   :: Expr a -> Expr a -> Expr a
  