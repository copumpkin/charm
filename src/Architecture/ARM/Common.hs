{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Architecture.ARM.Common where

import Prelude hiding (EQ)

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

data Hint = SY | ST | ISH | ISHST | OSH | OSHST | NSH | NSHST | InvalidHint Word8
  deriving (Show, Read, Eq)

data Direction = Decrement | Increment
  deriving (Show, Read, Eq, Enum)
  
data Order = Before | After
  deriving (Show, Read, Eq, Enum)

-- data Mode = ARM | Thumb | Jazelle | ThumbEE

negateCondition :: Condition -> Condition
negateCondition = toEnum . (xor 1) . fromEnum

data Conditional
data Unconditional

data GeneralInstruction a where
  Undefined     :: GeneralInstruction a
  Unconditional :: Instruction a Unconditional -> GeneralInstruction a
  Conditional   :: Condition -> Instruction a Conditional -> GeneralInstruction a

deriving instance (Show (Instruction a Conditional), Show (Instruction a Unconditional)) => Show (GeneralInstruction a)

data Z = Z
newtype S n = S n

data Specifier :: * -> * where
  Empty :: Specifier (S n)
  Then  :: Specifier n -> Specifier (S n)
  Else  :: Specifier n -> Specifier (S n)

deriving instance Show (Specifier n)

type ITSpecifier = Specifier (S (S (S (S Z))))

foldS :: a -> (a -> a) -> (a -> a) -> Specifier n -> a
foldS z t e Empty = z
foldS z t e (Then s) = t (foldS z t e s)
foldS z t e (Else s) = e (foldS z t e s)

itList :: Condition -> Specifier n -> [Condition]
itList c = reverse . foldS [c] (c:) (negateCondition c:)



generalInstruction :: r -> (Instruction a Unconditional -> r) -> (Condition -> Instruction a Conditional -> r) -> GeneralInstruction a -> r
generalInstruction ud _ _ Undefined = ud
generalInstruction _ uc _ (Unconditional i) = uc i
generalInstruction _ _  c (Conditional x i) = c x i

fromGeneralInstruction :: r -> (forall c. Instruction a c -> r) -> GeneralInstruction a -> r
fromGeneralInstruction ud f Undefined = ud
fromGeneralInstruction ud f (Unconditional i) = f i
fromGeneralInstruction ud f (Conditional _ i) = f i

class InstructionSet a where
  data Instruction a :: * -> *

type family Word a :: *




data GeneralDecoder a i = GeneralDecoder { _subarchs :: [Subarch]
                                         , _value    :: Word a
                                         , _mask     :: Word a
                                         , _decoder  :: Word a -> i
                                         }

decoderMatches :: Bits (Word e) => e -> Word e -> GeneralDecoder e i -> Bool
decoderMatches _ x (GeneralDecoder _ v m _) = x .&. m == v 

decode :: e -> Word e -> GeneralDecoder e i -> i
decode _ x (GeneralDecoder _ _ _ d) = d x


class Decoder e i where
  type Target e i :: *
  
  decoder :: [Subarch] -> Word e -> Word e -> (Word e -> i) -> GeneralDecoder e (Target e i)


