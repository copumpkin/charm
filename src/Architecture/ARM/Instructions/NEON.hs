{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}
module Architecture.ARM.Instructions.NEON where

import Prelude hiding (Integral, Floating)

import Architecture.ARM.Common

import Data.Word hiding (Word)
import Data.Int

data NEON = NEON

-- Yeah yeah, I know haskell isn't agda :( Horribly overengineered, but whatever

data Quadword
data Doubleword
data Singleword
data Halfword
data Byte

data Signed
data Unsigned

data Integral
data Floating

data Type :: * -> * -> * -> * where 
  S8  :: Type Signed   Integral Byte
  S16 :: Type Signed   Integral Halfword
  S32 :: Type Signed   Integral Singleword
  U8  :: Type Unsigned Integral Byte
  U16 :: Type Unsigned Integral Halfword
  U32 :: Type Unsigned Integral Singleword
  F32 :: Type Signed   Floating Singleword
  F64 :: Type Signed   Floating Doubleword

type family Repr s t w
type instance Repr Signed Integral Byte = Int8
type instance Repr Signed Integral Halfword = Int16
type instance Repr Signed Integral Singleword = Int32
type instance Repr Unsigned Integral Byte = Word8
type instance Repr Unsigned Integral Halfword = Word16
type instance Repr Unsigned Integral Singleword = Word32
type instance Repr Signed Floating Singleword = Float
type instance Repr Signed Floating Doubleword = Double


data VectorRegister :: * -> * where
  Q0  :: VectorRegister Quadword
  Q1  :: VectorRegister Quadword
  Q2  :: VectorRegister Quadword
  Q3  :: VectorRegister Quadword
  Q4  :: VectorRegister Quadword
  Q5  :: VectorRegister Quadword
  Q6  :: VectorRegister Quadword
  Q7  :: VectorRegister Quadword
  Q8  :: VectorRegister Quadword
  Q9  :: VectorRegister Quadword
  Q10 :: VectorRegister Quadword
  Q11 :: VectorRegister Quadword
  Q12 :: VectorRegister Quadword
  Q13 :: VectorRegister Quadword
  Q14 :: VectorRegister Quadword
  Q15 :: VectorRegister Quadword

  D0  :: VectorRegister Doubleword
  D1  :: VectorRegister Doubleword
  D2  :: VectorRegister Doubleword
  D3  :: VectorRegister Doubleword
  D4  :: VectorRegister Doubleword
  D5  :: VectorRegister Doubleword
  D6  :: VectorRegister Doubleword
  D7  :: VectorRegister Doubleword
  D8  :: VectorRegister Doubleword
  D9  :: VectorRegister Doubleword
  D10 :: VectorRegister Doubleword
  D11 :: VectorRegister Doubleword
  D12 :: VectorRegister Doubleword
  D13 :: VectorRegister Doubleword
  D14 :: VectorRegister Doubleword
  D15 :: VectorRegister Doubleword
  D16 :: VectorRegister Doubleword
  D17 :: VectorRegister Doubleword
  D18 :: VectorRegister Doubleword
  D19 :: VectorRegister Doubleword
  D20 :: VectorRegister Doubleword
  D21 :: VectorRegister Doubleword
  D22 :: VectorRegister Doubleword
  D23 :: VectorRegister Doubleword
  D24 :: VectorRegister Doubleword
  D25 :: VectorRegister Doubleword
  D26 :: VectorRegister Doubleword
  D27 :: VectorRegister Doubleword
  D28 :: VectorRegister Doubleword
  D29 :: VectorRegister Doubleword
  D30 :: VectorRegister Doubleword
  D31 :: VectorRegister Doubleword

instance InstructionSet NEON where
  data Instruction NEON c where
    VABA     :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VABAL    :: Type s t w -> VectorRegister Quadword -> VectorRegister Doubleword -> VectorRegister Doubleword -> Instruction NEON Conditional
    VABD     :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VABDL    :: Type s t w -> VectorRegister Quadword -> VectorRegister Doubleword -> VectorRegister Doubleword -> Instruction NEON Conditional
    VABS     :: Type s t w -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VACGE    :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VACGT    :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VADD     :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VADDHN   :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VADDL    :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VADDW    :: Type s t w -> VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VAND     :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VBIC     :: Type s t w -> VectorRegister q -> VectorRegister q -> Either (VectorRegister q) (Repr s t w) -> Instruction NEON Conditional
    VBIF     :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VBIT     :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VBSL     :: VectorRegister q -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VCEQ     :: Type s t w -> VectorRegister q -> VectorRegister q -> Maybe (VectorRegister q) -> Instruction NEON Conditional
    VCGE     :: Type s t w -> VectorRegister q -> VectorRegister q -> Maybe (VectorRegister q) -> Instruction NEON Conditional
    VCGT     :: Type s t w -> VectorRegister q -> VectorRegister q -> Maybe (VectorRegister q) -> Instruction NEON Conditional
    VCLE     :: Type s t w -> VectorRegister q -> VectorRegister q -> Maybe (VectorRegister q) -> Instruction NEON Conditional
    VCLS     :: Type s t w -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VCLT     :: Type s t w -> VectorRegister q -> VectorRegister q -> Maybe (VectorRegister q) -> Instruction NEON Conditional
    VCLZ     :: Type s t w -> VectorRegister q -> VectorRegister q -> Instruction NEON Conditional
    VCNT     :: Instruction NEON Conditional
    VCVT     :: Instruction NEON Conditional
    VDUP     :: Instruction NEON Conditional
    VEOR     :: Instruction NEON Conditional
    VEXT     :: Instruction NEON Conditional
    VFMA     :: Instruction NEON Conditional
    VFMS     :: Instruction NEON Conditional
    VHADD    :: Instruction NEON Conditional
    VHSUB    :: Instruction NEON Conditional
    VLD1     :: Instruction NEON Conditional
    VLD2     :: Instruction NEON Conditional
    VLD3     :: Instruction NEON Conditional
    VLD4     :: Instruction NEON Conditional
    VMAX     :: Instruction NEON Conditional
    VMIN     :: Instruction NEON Conditional
    VMLA     :: Instruction NEON Conditional
    VMLAL    :: Instruction NEON Conditional
    VMLS     :: Instruction NEON Conditional
    VMLSL    :: Instruction NEON Conditional
    VMOV     :: Instruction NEON Conditional
    VMOVL    :: Instruction NEON Conditional
    VMOVN    :: Instruction NEON Conditional
    VMUL     :: Instruction NEON Conditional
    VMULL    :: Instruction NEON Conditional
    VMVN     :: Instruction NEON Conditional
    VNEG     :: Instruction NEON Conditional
    VORN     :: Instruction NEON Conditional
    VORR     :: Instruction NEON Conditional
    VPADAL   :: Instruction NEON Conditional
    VPADD    :: Instruction NEON Conditional
    VPADDL   :: Instruction NEON Conditional
    VPMAX    :: Instruction NEON Conditional
    VPMIN    :: Instruction NEON Conditional
    VQABS    :: Instruction NEON Conditional
    VQADD    :: Instruction NEON Conditional
    VQDMLAL  :: Instruction NEON Conditional
    VQDMLSL  :: Instruction NEON Conditional
    VQDMULH  :: Instruction NEON Conditional
    VQDMULL  :: Instruction NEON Conditional
    VQMOVN   :: Instruction NEON Conditional
    VQMOVUN  :: Instruction NEON Conditional
    VQNEG    :: Instruction NEON Conditional
    VQRDMULH :: Instruction NEON Conditional
    VQRSHL   :: Instruction NEON Conditional
    VQRSHRN  :: Instruction NEON Conditional
    VQRSHRUN :: Instruction NEON Conditional
    VQSHL    :: Instruction NEON Conditional
    VQSHLU   :: Instruction NEON Conditional
    VQSHRN   :: Instruction NEON Conditional
    VQSHRUN  :: Instruction NEON Conditional
    VQSUB    :: Instruction NEON Conditional
    VRADDHN  :: Instruction NEON Conditional
    VRECPE   :: Instruction NEON Conditional
    VRECPS   :: Instruction NEON Conditional
    VREV16   :: Instruction NEON Conditional
    VREV32   :: Instruction NEON Conditional
    VREV64   :: Instruction NEON Conditional
    VRHADD   :: Instruction NEON Conditional
    VRSHL    :: Instruction NEON Conditional
    VRSHR    :: Instruction NEON Conditional
    VRSHRN   :: Instruction NEON Conditional
    VRSQRTE  :: Instruction NEON Conditional
    VRSQRTS  :: Instruction NEON Conditional
    VRSRA    :: Instruction NEON Conditional
    VRSUBHN  :: Instruction NEON Conditional
    VSHL     :: Instruction NEON Conditional
    VSHLL    :: Instruction NEON Conditional
    VSHR     :: Instruction NEON Conditional
    VSHRN    :: Instruction NEON Conditional
    VSLI     :: Instruction NEON Conditional
    VSRA     :: Instruction NEON Conditional
    VSRI     :: Instruction NEON Conditional
    VST1     :: Instruction NEON Conditional
    VST2     :: Instruction NEON Conditional
    VST3     :: Instruction NEON Conditional
    VST4     :: Instruction NEON Conditional
    VSUB     :: Instruction NEON Conditional
    VSUBHN   :: Instruction NEON Conditional
    VSUBL    :: Instruction NEON Conditional
    VSUBW    :: Instruction NEON Conditional
    VSWP     :: Instruction NEON Conditional
    VTBL     :: Instruction NEON Conditional
    VTBX     :: Instruction NEON Conditional
    VTRN     :: Instruction NEON Conditional
    VTST     :: Instruction NEON Conditional
    VUZP     :: Instruction NEON Conditional
    VZIP     :: Instruction NEON Conditional

