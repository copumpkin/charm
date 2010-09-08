module Architecture.ARM.State where

import Prelude hiding ((.))

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Record.Label

import Control.Category

import Architecture.ARM.Common


data ARMState r m = ARMState { _regs       :: Map ARMRegister r -- An EnumMap?
                             , _memory     :: IntMap m -- A trie or interval-tree-based memory map, when I get around to writing one
                             , _statusRegs :: Map ARMStatusRegister r
                             }

getReg :: ARMRegister -> ARMState r m -> r
getReg r s = _regs s Map.! r

setReg :: ARMRegister -> r -> ARMState r m -> ARMState r m
setReg r x s = s { _regs = Map.insert r x (_regs s) }

reg r = lens (getReg r) (setReg r)

r0  = reg R0
r1  = reg R1
r2  = reg R2
r3  = reg R3
r4  = reg R4
r5  = reg R5
r6  = reg R6
r7  = reg R7
r8  = reg R8
r9  = reg R9
r10 = reg R10
r11 = reg R11
r12 = reg R12
sp  = reg SP
lr  = reg LR
pc  = reg PC

getMem :: Word32 -> ARMState r m -> m
getMem p s = _memory s IntMap.! fromIntegral p

setMem :: Word32 -> m -> ARMState r m -> ARMState r m
setMem p x s = s { _memory = IntMap.insert (fromIntegral p) x (_memory s) }

memory p = lens (getMem p) (setMem p)