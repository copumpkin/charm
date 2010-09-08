{-# LANGUAGE GADTs #-}
module Architecture.ARM.Instructions.UAL.Semantics where
  
import Architecture.ARM.State
import Architecture.ARM.Instructions.UAL

import Data.Record.Label

-- This should be put somewhere else, maybe even in a separate package
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

-- Num should be an expression, and we should steal all the operators
-- Need state monad
evaluateConditional :: Num r => Conditional -> ARMState r m -> ARMState r m
evaluateConditional (B off) = modL pc (+ fromIntegral off)
evaluateConditional (BX r)  = do x <- getL (reg r)
                                 setL pc x
                                 -- set arm/thumb bit
evaluateConditional x = error $ "haven't implemented semantics for conditional instruction " ++ show x

evaluate :: UALInstruction -> ARMState r m -> ARMState r m
evaluate x = error $ "haven't implemented semantics for instruction " ++ show x