{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, EmptyDataDecls, FlexibleContexts #-}
module Architecture.ARM.Common where

import Data.Bits
import Data.Word hiding (Word)
import Data.Int

data Subarch = ARM_EXT_V1
             | ARM_EXT_V2
             | ARM_EXT_V2S
             | ARM_EXT_V3
             | ARM_EXT_V3M
             | ARM_EXT_V4T 
             | ARM_EXT_V5
             | ARM_EXT_V5T
             | ARM_EXT_V5E
             | ARM_EXT_V5ExP
             | ARM_EXT_V5J
             | ARM_EXT_V6
             | ARM_EXT_V6K
             | ARM_EXT_V6T2
             | ARM_EXT_V6Z
             | ARM_EXT_V7
             | ARM_EXT_DIV
             | ARM_CEXT_IWMMXT
             | ARM_CEXT_MAVERICK
             | ARM_CEXT_XSCALE
             | FPU_FPA_EXT_V1
             | FPU_NEON_EXT_V1
             | FPU_NEON_FP16
             | FPU_VFP_EXT_V1xD
             | FPU_VFP_EXT_V2
             | FPU_VFP_EXT_V3

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
              | R9 | R10 | R11 | R12 | SP | LR | PC
  deriving (Show, Read, Eq, Ord, Enum)

data Shift = S_LSL | S_LSR | S_ASR | S_ROR
  deriving (Show, Read, Eq, Enum)

data Condition = EQ | NE | CS | CC | MI | PL | VS | VC
               | HI | LS | GE | LT | GT | LE | AL | UND
  deriving (Show, Read, Eq, Enum)

data StatusRegister = CPSR | SPSR
  deriving (Show, Read, Eq, Ord, Enum)

data Endianness = Big | Little
  deriving (Show, Read, Eq, Enum)

data Nybble = High | Low
  deriving (Show, Read, Eq, Enum)

data Width = Byte | Halfword | Word | Doubleword
  deriving (Show, Read, Eq, Enum)

data Hint = SY | UN | ST | UNST | UK Word32 -- FIXME: should really prefix these consistently
  deriving (Show, Read, Eq)

data Direction = Decrement | Increment
  deriving (Show, Read, Eq, Enum)
  
data Order = Before | After
  deriving (Show, Read, Eq, Enum)

-- data Mode = ARM | Thumb | Jazelle | ThumbEE


data Conditional
data Unconditional


data GeneralInstruction a where
  Conditional   :: Condition -> Instruction a Conditional -> GeneralInstruction a
  Unconditional :: Instruction a Unconditional -> GeneralInstruction a
  Undefined     :: GeneralInstruction a

class InstructionSet a where
  data Instruction a :: * -> *

type family Word a :: *




data GeneralDecoder w i = GeneralDecoder { _subarchs :: [Subarch]
                                         , _value    :: w
                                         , _mask     :: w
                                         , _decoder  :: w -> i
                                         }

decoderMatches :: Bits (Word e) => e -> Word e -> GeneralDecoder (Word e) i -> Bool
decoderMatches _ x (GeneralDecoder _ v m _) = x .&. m == v 

decode :: e -> Word e -> GeneralDecoder (Word e) i -> i
decode _ x (GeneralDecoder _ _ _ d) = d x


class Decoder e i where
  type DecoderType e i :: *
  
  decoder :: e -> [Subarch] -> Word e -> Word e -> (Word e -> i) -> DecoderType e i

