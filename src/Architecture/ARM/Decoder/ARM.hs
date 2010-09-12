{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}
module Architecture.ARM.Decoder.ARM where

import Prelude hiding (and)

import Architecture.ARM.Common
import Architecture.ARM.Instructions.UAL

import Data.Maybe
import Data.List hiding (and)
import Data.Int
import Data.Word hiding (Word)
import Data.Bits hiding (bit)

import Text.Printf

import Control.Monad
import Control.Applicative

data ARM = ARM
type instance Word ARM = Word32

type D a = Word32 -> a

condition = arm_c

instance Decoder ARM (Instruction UAL Conditional) where
  type DecoderType ARM (Instruction UAL Conditional) = GeneralDecoder (Word ARM) (GeneralInstruction UAL)

  decoder e s v m d = GeneralDecoder s v m (Conditional <$> condition <*> d)

instance Decoder ARM (Instruction UAL Unconditional) where
  type DecoderType ARM (Instruction UAL Unconditional) = GeneralDecoder (Word ARM) (GeneralInstruction UAL)
  
  decoder e s v m d = GeneralDecoder s v m (Unconditional <$> d)

instance Decoder a (GeneralInstruction i) where
  type DecoderType a (GeneralInstruction i) = GeneralDecoder (Word a) (GeneralInstruction i)
  
  decoder e s v m d = GeneralDecoder s v m d



bitRange :: (Integral a, Bits a, Integral b) => Int -> Int -> a -> b
bitRange start end i = fromIntegral ((((fromIntegral i :: Integer) `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1)))

allSet :: Int -> Int -> Word32 -> Bool
allSet start end = (== (((2 :: Word32) `shiftL` (end - start)) - 1)) . bitRange start end

noneSet :: Int -> Int -> Word32 -> Bool
noneSet start end = (== 0) . bitRange start end

flatten :: MemOp -> MemOp
flatten (MemRegNeg x (Imm z) y) = MemReg x (Imm $ negate z) y
flatten (MemRegPostNeg x (Imm z)) = MemRegPost x (Imm $ negate z)
flatten x = x

armDecodeAddress :: D MemOp
armDecodeAddress =
  do flag   <- bool 25
     post   <- not <$> bool 24
     neg    <- not <$> bool 23
     offset <- integral 0 11
     base   <- reg 16
     let memReg = if neg then MemRegNeg else MemReg
         memRegPost = if neg then MemRegPostNeg else MemRegPost
         mem = if post then (const .) . memRegPost else memReg
     if base == PC && not flag -- is this special case necessary?
       then if post
              then return $ MemRegPost PC (Imm offset) -- Is this right? post on PC?
              else memReg PC (Imm offset) <$> bool 21
       else if flag
              then mem base <$> armDecodeShift <*> bool 21 
              else mem base (Imm offset) <$> bool 21


armDecodeShift :: D DataOp
armDecodeShift i = let shift = toEnum $ integral 5 6 i in
                     if not (noneSet 4 11 i) then
                       if not (bool 4 i) then
                         let amount = integral 7 11 i in
                           if amount == 0 && shift == S_ROR
                             then RegShiftRRX (reg 0 i)
                             else RegShiftImm shift (if amount == 0 then 32 else amount) (reg 0 i)
                         else RegShiftReg shift (reg 8 i) (reg 0 i)
                       else Reg (reg 0 i)

arm_a :: D MemOp
arm_a = flatten . armDecodeAddress

arm_s' :: D MemOp
arm_s' =
  do neg       <- not <$> bool 23
     pre       <- bool 24
     imm       <- bool 22
     offset    <- liftM2 (.|.) ((`shiftL` 4) . integral 8 11) (integral 0 3)
     
     -- Do I need special cases?
     let memRegPre = if neg then MemRegNeg else MemReg
         memRegPost = if neg then MemRegPostNeg else MemRegPost
         memReg = if pre then memRegPre else (const .) . memRegPost
         final = memReg <$> reg 16 <*> (if imm then pure (Imm offset) else Reg <$> reg 0) <*> bool 21
     
     final
             
arm_s :: D MemOp
arm_s = flatten . arm_s'

-- this needs a sign extend function
arm_b :: D Int32
arm_b i = ((((fromIntegral i :: Int32) .&. 0xffffff) `xor` 0x800000) - 0x800000) * 4 {- FIXME: not here -} + 8

arm_c :: D Condition
arm_c = toEnum . integral 28 31

arm_m :: D [Register]
arm_m i = [toEnum b | b <- [0..15], bool b i]

arm_o :: D DataOp
arm_o i | bool 25 i = Imm $ integral 0 7 i `rotateR` (2 * integral 8 11 i)
        | otherwise = armDecodeShift i

arm_p :: D Bool
arm_p = allSet 12 15

arm_t :: D Bool
arm_t = liftA2 (&&) (bool 21) (not <$> bool 24)

arm_q :: D DataOp
arm_q i = armDecodeShift i

arm_e :: D Word32
arm_e = liftA2 (.|.) ((`shiftL` 4) <$> integral 8 19) (integral 0 3)

arm_B :: D Int32
arm_B = do x <- choose 23 0 0xff000000 -- negative bit
           y <- integral 0 23
           let offset = (x + y :: Int32) `shiftL` 2
           z <- choose 24 0 2 -- 2
           return (fromIntegral (offset + 8 + z)) -- FIXME: Do I want that +8 in there? Or should I deal with it later. Probably later, but will leave it in for now.

-- FIXME: this is ugly
arm_C :: D String
arm_C i = '_' : (if i .&. 0x80000 /= 0 then "f" else "" ++ 
                 if i .&. 0x40000 /= 0 then "s" else "" ++
                 if i .&. 0x20000 /= 0 then "x" else "" ++
                 if i .&. 0x10000 /= 0 then "c" else "")

arm_U :: D Hint
arm_U i = case i .&. 0xf of
            0xf -> SY
            0x7 -> UN
            0xe -> ST
            0x6 -> UNST
            x   -> UK x

arm_P :: D MemOp
arm_P i = armDecodeAddress $ i .|. (1 `shiftL` 24)

reg :: Int -> D Register
reg start i = toEnum (bitRange start (start + 3) i)

integral :: (Integral a, Bits a) => Int -> Int -> D a
integral start end i = bitRange start end i

integral' :: (Integral a, Bits a) => Int -> Int -> D a
integral' start end i = (+1) . bitRange start end $ i

arm_E :: D (Maybe (Word32, Word32))
arm_E i = let msb = (i .&. 0x1f0000) `shiftR` 16
              lsb = (i .&. 0xf80) `shiftR` 7
              width = msb - lsb + 1 in
            if width > 0 then
              Just (lsb, width) --"#" ++ (show lsb) ++ ", #" ++ (show width)
              else Nothing --"(invalid " ++ (show lsb) ++ ":" ++ (show msb) ++ ")"            

arm_V :: D Word32
arm_V = liftM2 (.|.) ((`shiftL` 12) <$> integral 16 19) (integral 0 11)

bit b i = bitRange b b i

bool b s = bit b s == 1

enum :: (Integral i, Enum a) => i -> a
enum = toEnum . fromIntegral

arm_bw bit = choose bit Word Byte 
arm_bh bit = choose bit Byte Halfword

reg12_reg0_reg16 f = f <$> reg 12 <*> reg 0 <*> reg 16

reg12_reg16_reg0 f = f <$> reg 12 <*> reg 16 <*> reg 0
reg12_reg16_reg0_reg8 f = reg12_reg16_reg0 f <*> reg 8
reg16_reg0_reg8 f = f <$> reg 16 <*> reg 0 <*> reg 8
reg16_reg0_reg8_reg12 f = reg16_reg0_reg8 f <*> reg 12

direction :: Int -> D Direction
direction n = choose n Decrement Increment

order :: Int -> D Order
order n = choose n After Before

choose :: Int -> a -> a -> Word32 -> a
choose n t f x = if not (bool n x) then t else f

bool20_reg12_reg16_o f = f <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o


armDecoders = 
  [ decoder ARM [ARM_EXT_V4T, ARM_EXT_V5] 0x012FFF10 0x0ffffff0 (BX <$> reg 0)
  , decoder ARM [ARM_EXT_V2]    0x00000090 0x0fe000f0 (mul <$> bool 20 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V2]    0x00200090 0x0fe000f0 (mla <$> bool 20 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder ARM [ARM_EXT_V2S]   0x01000090 0x0fb00ff0 (swp <$> bool 22 <*> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False))
  , decoder ARM [ARM_EXT_V3M]   0x00800090 0x0fa000f0 (choose 22 umull smull <*> bool 20 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V3M]   0x00a00090 0x0fa000f0 (choose 22 umlal smlal <*> bool 20 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)

  , decoder ARM [ARM_EXT_V7]    0xf450f000 0xfd70f000 (PLI <$> arm_P)
  , decoder ARM [ARM_EXT_V7]    0x0320f0f0 0x0ffffff0 (DBG <$> integral 0 3)
  , decoder ARM [ARM_EXT_V7]    0xf57ff050 0x0ffffff0 (DMB <$> arm_U)
  , decoder ARM [ARM_EXT_V7]    0xf57ff040 0x0ffffff0 (DSB <$> arm_U)
  , decoder ARM [ARM_EXT_V7]    0xf57ff060 0x0ffffff0 (ISB <$> arm_U)

  , decoder ARM [ARM_EXT_V6T2]  0x07c0001f 0x0fe0007f (BFC <$> reg 12 <*> arm_E)
  , decoder ARM [ARM_EXT_V6T2]  0x07c00010 0x0fe00070 (BFI <$> reg 12 <*> reg 0 <*> arm_E)
  , decoder ARM [ARM_EXT_V6T2]  0x00600090 0x0ff000f0 (MLS <$> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder ARM [ARM_EXT_V6T2]  0x006000b0 0x0f7000f0 (STRHT <$> reg 12 <*> arm_s) -- TODO: check me


  , decoder ARM [ARM_EXT_V6T2]  0x00300090 0x0f3000f0 (pure Undefined)
  , decoder ARM [ARM_EXT_V6T2]  0x00300090 0x0f300090 (ldr <$> arm_bh 5 <*> pure True <*> bool 6 <*> reg 12 <*> arm_s)

  , decoder ARM [ARM_EXT_V6T2]  0x03000000 0x0ff00000 (MOVW <$> reg 12 <*> arm_V)
  , decoder ARM [ARM_EXT_V6T2]  0x03400000 0x0ff00000 (MOVT <$> reg 12 <*> arm_V)
  , decoder ARM [ARM_EXT_V6T2]  0x06ff0f30 0x0fff0ff0 (RBIT <$> reg 12 <*> reg 0)
  , decoder ARM [ARM_EXT_V6T2]  0x07a00050 0x0fa00070 (choose 22 SBFX UBFX <*> reg 12 <*> reg 0 <*> integral 7 11 <*> integral' 16 20)

  , decoder ARM [ARM_EXT_V6Z]   0x01600070 0x0ff000f0 (SMC <$> arm_e)

  , decoder ARM [ARM_EXT_V6K]   0xf57ff01f 0xffffffff (pure CLREX) 
  , decoder ARM [ARM_EXT_V6K]   0x01d00f9f 0x0ff00fff (LDREXB <$> reg 12 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False))
  , decoder ARM [ARM_EXT_V6K]   0x01b00f9f 0x0ff00fff (do rt <- reg 12; rn <- reg 16; return (LDREXD rt (succ rt) (MemReg rn (Imm 0) False))) -- Doesn't really need to be this compliated. We could just have the second argument be implicit (but that makes things a little uglier to work with later)
  , decoder ARM [ARM_EXT_V6K]   0x01f00f9f 0x0ff00fff (LDREXH <$> reg 12 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False))
  , decoder ARM [ARM_EXT_V6K]   0x01c00f90 0x0ff00ff0 (STREXB <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False))
  , decoder ARM [ARM_EXT_V6K]   0x01a00f90 0x0ff00ff0 (do rd <- reg 12; rn <- reg 16; rt <- reg 0; return (STREXD rd rt (succ rt) (MemReg rn (Imm 0) False))) -- As above
  , decoder ARM [ARM_EXT_V6K]   0x01e00f90 0x0ff00ff0 (STREXH <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False))

  , decoder ARM [ARM_EXT_V6K]   0x0320f001 0x0fffffff (pure YIELD)
  , decoder ARM [ARM_EXT_V6K]   0x0320f002 0x0fffffff (pure WFE)
  , decoder ARM [ARM_EXT_V6K]   0x0320f003 0x0fffffff (pure WFI)
  , decoder ARM [ARM_EXT_V6K]   0x0320f004 0x0fffffff (pure SEV)
  , decoder ARM [ARM_EXT_V6K]   0x0320f000 0x0fffff00 (pure NOP)
  
  , decoder ARM [ARM_EXT_V6]    0xf1080000 0xfffffe3f (CPSIE <$> bool 8 <*> bool 7 <*> bool 6 <*> pure Nothing)
  , decoder ARM [ARM_EXT_V6]    0xf10a0000 0xfffffe20 (CPSIE <$> bool 8 <*> bool 7 <*> bool 6 <*> (Just <$> integral 0 4))
  , decoder ARM [ARM_EXT_V6]    0xf10C0000 0xfffffe3f (CPSID <$> bool 8 <*> bool 7 <*> bool 6 <*> pure Nothing)
  , decoder ARM [ARM_EXT_V6]    0xf10e0000 0xfffffe20 (CPSID <$> bool 8 <*> bool 7 <*> bool 6 <*> (Just <$> integral 0 4))
  , decoder ARM [ARM_EXT_V6]    0xf1000000 0xfff1fe20 (CPS <$> integral 0 4)

  , decoder ARM [ARM_EXT_V6]    0x06800010 0x0ff00ff0 (PKHBT <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800010 0x0ff00070 (PKHBT <$> reg 12 <*> reg 16 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800050 0x0ff00ff0 (PKHTB <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ASR 32 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800050 0x0ff00070 (PKHTB <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x01900f9f 0x0ff00fff (LDREX  <$> reg 12 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False) )
  , decoder ARM [ARM_EXT_V6]    0x06200f10 0x0ff00ff0 (reg12_reg16_reg0 $ QADD16)
  , decoder ARM [ARM_EXT_V6]    0x06200f90 0x0ff00ff0 (reg12_reg16_reg0 $ QADD8)
  , decoder ARM [ARM_EXT_V6]    0x06200f30 0x0ff00ff0 (reg12_reg16_reg0 $ QASX)
  , decoder ARM [ARM_EXT_V6]    0x06200f70 0x0ff00ff0 (reg12_reg16_reg0 $ QSUB16)
  , decoder ARM [ARM_EXT_V6]    0x06200ff0 0x0ff00ff0 (reg12_reg16_reg0 $ QSUB8)
  , decoder ARM [ARM_EXT_V6]    0x06200f50 0x0ff00ff0 (reg12_reg16_reg0 $ QSAX)
  , decoder ARM [ARM_EXT_V6]    0x06100f10 0x0ff00ff0 (reg12_reg16_reg0 $ SADD16)
  , decoder ARM [ARM_EXT_V6]    0x06100f90 0x0ff00ff0 (reg12_reg16_reg0 $ SADD8)
  , decoder ARM [ARM_EXT_V6]    0x06100f30 0x0ff00ff0 (reg12_reg16_reg0 $ SASX)
  , decoder ARM [ARM_EXT_V6]    0x06300f10 0x0ff00ff0 (reg12_reg16_reg0 $ SHADD16)
  , decoder ARM [ARM_EXT_V6]    0x06300f90 0x0ff00ff0 (reg12_reg16_reg0 $ SHADD8)
  , decoder ARM [ARM_EXT_V6]    0x06300f30 0x0ff00ff0 (reg12_reg16_reg0 $ SHASX)
  , decoder ARM [ARM_EXT_V6]    0x06300f70 0x0ff00ff0 (reg12_reg16_reg0 $ SHSUB16)
  , decoder ARM [ARM_EXT_V6]    0x06300ff0 0x0ff00ff0 (reg12_reg16_reg0 $ SHSUB8)
  , decoder ARM [ARM_EXT_V6]    0x06300f50 0x0ff00ff0 (reg12_reg16_reg0 $ SHSAX)
  , decoder ARM [ARM_EXT_V6]    0x06100f70 0x0ff00ff0 (reg12_reg16_reg0 $ SSUB16)
  , decoder ARM [ARM_EXT_V6]    0x06100ff0 0x0ff00ff0 (reg12_reg16_reg0 $ SSUB8)
  , decoder ARM [ARM_EXT_V6]    0x06100f50 0x0ff00ff0 (reg12_reg16_reg0 $ SSAX)
  , decoder ARM [ARM_EXT_V6]    0x06500f10 0x0ff00ff0 (reg12_reg16_reg0 $ UADD16)
  , decoder ARM [ARM_EXT_V6]    0x06500f90 0x0ff00ff0 (reg12_reg16_reg0 $ UADD8)
  , decoder ARM [ARM_EXT_V6]    0x06500f30 0x0ff00ff0 (reg12_reg16_reg0 $ UASX)
  , decoder ARM [ARM_EXT_V6]    0x06700f10 0x0ff00ff0 (reg12_reg16_reg0 $ UHADD16)
  , decoder ARM [ARM_EXT_V6]    0x06700f90 0x0ff00ff0 (reg12_reg16_reg0 $ UHADD8)
  , decoder ARM [ARM_EXT_V6]    0x06700f30 0x0ff00ff0 (reg12_reg16_reg0 $ UHASX)
  , decoder ARM [ARM_EXT_V6]    0x06700f70 0x0ff00ff0 (reg12_reg16_reg0 $ UHSUB16)
  , decoder ARM [ARM_EXT_V6]    0x06700ff0 0x0ff00ff0 (reg12_reg16_reg0 $ UHSUB8)
  , decoder ARM [ARM_EXT_V6]    0x06700f50 0x0ff00ff0 (reg12_reg16_reg0 $ UHSAX)
  , decoder ARM [ARM_EXT_V6]    0x06600f10 0x0ff00ff0 (reg12_reg16_reg0 $ UQADD16)
  , decoder ARM [ARM_EXT_V6]    0x06600f90 0x0ff00ff0 (reg12_reg16_reg0 $ UQADD8)
  , decoder ARM [ARM_EXT_V6]    0x06600f30 0x0ff00ff0 (reg12_reg16_reg0 $ UQASX)
  , decoder ARM [ARM_EXT_V6]    0x06600f70 0x0ff00ff0 (reg12_reg16_reg0 $ UQSUB16)
  , decoder ARM [ARM_EXT_V6]    0x06600ff0 0x0ff00ff0 (reg12_reg16_reg0 $ UQSUB8)
  , decoder ARM [ARM_EXT_V6]    0x06600f50 0x0ff00ff0 (reg12_reg16_reg0 $ UQSAX)
  , decoder ARM [ARM_EXT_V6]    0x06500f70 0x0ff00ff0 (reg12_reg16_reg0 $ USUB16)
  , decoder ARM [ARM_EXT_V6]    0x06500ff0 0x0ff00ff0 (reg12_reg16_reg0 $ USUB8)
  , decoder ARM [ARM_EXT_V6]    0x06500f50 0x0ff00ff0 (reg12_reg16_reg0 $ USAX) 
  , decoder ARM [ARM_EXT_V6]    0x06bf0f30 0x0fff0ff0 (REV     <$> reg 12 <*> reg 0)
  , decoder ARM [ARM_EXT_V6]    0x06bf0fb0 0x0fff0ff0 (REV16   <$> reg 12 <*> reg 0)
  , decoder ARM [ARM_EXT_V6]    0x06ff0fb0 0x0fff0ff0 (REVSH   <$> reg 12 <*> reg 0)
  , decoder ARM [ARM_EXT_V6]    0xf8100a00 0xfe50ffff (rfe <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16)
  , decoder ARM [ARM_EXT_V6]    0x06bf0070 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06bf0470 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06bf0870 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06bf0c70 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x068f0070 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x068f0470 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x068f0870 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x068f0c70 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06af0070 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06af0470 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06af0870 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06af0c70 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ff0070 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ff0470 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ff0870 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ff0c70 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06cf0070 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06cf0470 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06cf0870 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06cf0c70 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ef0070 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ef0470 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ef0870 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06ef0c70 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06b00070 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06b00470 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06b00870 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06b00c70 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800070 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800470 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800870 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800c70 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00070 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00470 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00870 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00c70 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06f00070 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06f00470 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06f00870 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06f00c70 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06c00070 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06c00470 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06c00870 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06c00c70 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00070 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00470 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00870 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00c70 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06800fb0 0x0ff00ff0 (reg12_reg16_reg0 $ SEL)
  , decoder ARM [ARM_EXT_V6]    0xf1010000 0xfffffc00 (SETEND <$> choose 9 Big Little)
  , decoder ARM [ARM_EXT_V6]    0x0700f010 0x0ff0f0d0 (smuad  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8) -- TODO: double check enum direction is correct for first arg
  , decoder ARM [ARM_EXT_V6]    0x0700f050 0x0ff0f0d0 (smusd  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V6]    0x07000010 0x0ff000d0 (smlad  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder ARM [ARM_EXT_V6]    0x07400010 0x0ff000d0 (smlald <$> enum . bit 5 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V6]    0x07000050 0x0ff000d0 (smlsd  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12) 
  , decoder ARM [ARM_EXT_V6]    0x07400050 0x0ff000d0 (smlsld <$> enum . bit 5 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V6]    0x0750f010 0x0ff0f0d0 (smmul  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V6]    0x07500010 0x0ff000d0 (smmla  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder ARM [ARM_EXT_V6]    0x075000d0 0x0ff000d0 (smmls  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder ARM [ARM_EXT_V6]    0xf84d0500 0xfe5fffe0 (srs  <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> integral 0 4)
  , decoder ARM [ARM_EXT_V6]    0x06a00010 0x0fe00ff0 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00010 0x0fe00070 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00050 0x0fe00070 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06a00f30 0x0ff00ff0 (SSAT16 <$> reg 12 <*> integral' 16 19 <*> reg 0)
  , decoder ARM [ARM_EXT_V6]    0x01800f90 0x0ff00ff0 (STREX  <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False) )
  , decoder ARM [ARM_EXT_V6]    0x00400090 0x0ff000f0 (UMAAL  <$> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder ARM [ARM_EXT_V6]    0x0780f010 0x0ff0f0f0 (reg16_reg0_reg8 $ USAD8 )
  , decoder ARM [ARM_EXT_V6]    0x07800010 0x0ff000f0 (reg16_reg0_reg8_reg12 $ USADA8)
  , decoder ARM [ARM_EXT_V6]    0x06e00010 0x0fe00ff0 (USAT   <$> reg 12 <*> integral 16 20 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00010 0x0fe00070 (USAT   <$> reg 12 <*> integral 16 20 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00050 0x0fe00070 (USAT   <$> reg 12 <*> integral 16 20 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder ARM [ARM_EXT_V6]    0x06e00f30 0x0ff00ff0 (USAT16 <$> reg 12 <*> integral 16 19 <*> reg 0)
 
  , decoder ARM [ARM_EXT_V5J]   0x012fff20 0x0ffffff0 (BXJ <$> reg 0)
 
  , decoder ARM [ARM_EXT_V5]    0xe1200070 0xfff000f0 (BKPT <$> ((\x y -> x `shiftL` 4 .|. y) <$> integral 8 19 <*> integral 0 3)) 
  , decoder ARM [ARM_EXT_V5]    0xfa000000 0xfe000000 (BLXUC <$> arm_B)
  , decoder ARM [ARM_EXT_V5]    0x012fff30 0x0ffffff0 (BLX <$> reg 0)
  , decoder ARM [ARM_EXT_V5]    0x016f0f10 0x0fff0ff0 (CLZ <$> reg 12 <*> reg 0)
  
  , decoder ARM [ARM_EXT_V5E]   0x000000d0 0x0e1000f0 (LDRD <$> reg 12 <*> arm_s)
  , decoder ARM [ARM_EXT_V5E]   0x000000f0 0x0e1000f0 (STRD <$> reg 12 <*> arm_s)
  , decoder ARM [ARM_EXT_V5E]   0xf450f000 0xfc70f000 (PLD <$> arm_a)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x01000080 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLABB)
  , decoder ARM [ARM_EXT_V5ExP] 0x010000a0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLATB)
  , decoder ARM [ARM_EXT_V5ExP] 0x010000c0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLABT)
  , decoder ARM [ARM_EXT_V5ExP] 0x010000e0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLATT)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x01200080 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLAWB)
  , decoder ARM [ARM_EXT_V5ExP] 0x012000c0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLAWT)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALBB)
  , decoder ARM [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALTB)
  , decoder ARM [ARM_EXT_V5ExP] 0x014000c0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALBT)
  , decoder ARM [ARM_EXT_V5ExP] 0x014000e0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALTT)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x01600080 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULBB)
  , decoder ARM [ARM_EXT_V5ExP] 0x016000a0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULTB)
  , decoder ARM [ARM_EXT_V5ExP] 0x016000c0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULBT)
  , decoder ARM [ARM_EXT_V5ExP] 0x016000e0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULTT)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x012000a0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULWB)
  , decoder ARM [ARM_EXT_V5ExP] 0x012000e0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULWT)
  
  , decoder ARM [ARM_EXT_V5ExP] 0x01000050 0x0ff00ff0 (reg12_reg0_reg16 $ QADD)
  , decoder ARM [ARM_EXT_V5ExP] 0x01400050 0x0ff00ff0 (reg12_reg0_reg16 $ QDADD)
  , decoder ARM [ARM_EXT_V5ExP] 0x01200050 0x0ff00ff0 (reg12_reg0_reg16 $ QSUB)
  , decoder ARM [ARM_EXT_V5ExP] 0x01600050 0x0ff00ff0 (reg12_reg0_reg16 $ QDSUB)
  
  -- This matches arm-dis.c, but is it right? It doesn't match encoding A1 or A2 in the reference
  , decoder ARM [ARM_EXT_V1]    0x052d0004 0x0fff0fff (PUSH <$> Regs <$> (pure <$> reg 12)) 
                            
  , decoder ARM [ARM_EXT_V1]    0x04400000 0x0e500000 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x04000000 0x0e500000 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
  , decoder ARM [ARM_EXT_V1]    0x06400000 0x0e500ff0 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x06000000 0x0e500ff0 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
  , decoder ARM [ARM_EXT_V1]    0x04400000 0x0c500010 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x04000000 0x0c500010 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
                            
  , decoder ARM [ARM_EXT_V1]    0x04400000 0x0e500000 (STRB <$> reg 12 <*> arm_a) -- "strb%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x06400000 0x0e500010 (STRB <$> reg 12 <*> arm_a) -- "strb%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x004000b0 0x0e5000f0 (STRH <$> reg 12 <*> arm_s) -- "strh%c\t%12-15R, %s"},
  , decoder ARM [ARM_EXT_V1]    0x000000b0 0x0e500ff0 (STRH <$> reg 12 <*> arm_s) -- "strh%c\t%12-15R, %s"},
                            
  , decoder ARM [ARM_EXT_V1]    0x00500090 0x0e5000f0 (pure Undefined)
  , decoder ARM [ARM_EXT_V1]    0x00500090 0x0e500090 (ldr <$> arm_bh 5 <*> pure False <*> bool 6 <*> reg 12 <*> arm_s) -- "ldr%6's%5?hb%c\t%12-15R, %s"},
  , decoder ARM [ARM_EXT_V1]    0x00100090 0x0e500ff0 (pure Undefined)
  , decoder ARM [ARM_EXT_V1]    0x00100090 0x0e500f90 (ldr <$> arm_bh 5 <*> pure False <*> bool 6 <*> reg 12 <*> arm_s) -- "ldr%6's%5?hb%c\t%12-15R, %s"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02000000 0x0fe00000 (bool20_reg12_reg16_o $ and)  -- "and%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00000000 0x0fe00010 (bool20_reg12_reg16_o $ and)  -- "and%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00000010 0x0fe00090 (bool20_reg12_reg16_o $ and)  -- "and%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02200000 0x0fe00000 (bool20_reg12_reg16_o $ eor)  -- "eor%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00200000 0x0fe00010 (bool20_reg12_reg16_o $ eor)  -- "eor%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00200010 0x0fe00090 (bool20_reg12_reg16_o $ eor)  -- "eor%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02400000 0x0fe00000 (bool20_reg12_reg16_o $ sub)  -- "sub%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00400000 0x0fe00010 (bool20_reg12_reg16_o $ sub)  -- "sub%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00400010 0x0fe00090 (bool20_reg12_reg16_o $ sub)  -- "sub%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02600000 0x0fe00000 (bool20_reg12_reg16_o $ rsb)  -- "rsb%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00600000 0x0fe00010 (bool20_reg12_reg16_o $ rsb)  -- "rsb%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00600010 0x0fe00090 (bool20_reg12_reg16_o $ rsb)  -- "rsb%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02800000 0x0fe00000 (bool20_reg12_reg16_o $ add)  -- "add%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00800000 0x0fe00010 (bool20_reg12_reg16_o $ add)  -- "add%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00800010 0x0fe00090 (bool20_reg12_reg16_o $ add)  -- "add%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02a00000 0x0fe00000 (bool20_reg12_reg16_o $ adc)  -- "adc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00a00000 0x0fe00010 (bool20_reg12_reg16_o $ adc)  -- "adc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00a00010 0x0fe00090 (bool20_reg12_reg16_o $ adc)  -- "adc%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02c00000 0x0fe00000 (bool20_reg12_reg16_o $ sbc)  -- "sbc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00c00000 0x0fe00010 (bool20_reg12_reg16_o $ sbc)  -- "sbc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00c00010 0x0fe00090 (bool20_reg12_reg16_o $ sbc)  -- "sbc%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V1]    0x02e00000 0x0fe00000 (bool20_reg12_reg16_o $ rsc)  -- "rsc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00e00000 0x0fe00010 (bool20_reg12_reg16_o $ rsc)  -- "rsc%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x00e00010 0x0fe00090 (bool20_reg12_reg16_o $ rsc)  -- "rsc%20's%c\t%12-15R, %16-19R, %o"},
                            
  , decoder ARM [ARM_EXT_V3]    0x0120f000 0x0db0f000 (MSR <$> bool 18 <*> bool 19 <*> arm_o)
  , decoder ARM [ARM_EXT_V3]    0x010f0000 0x0fbf0fff (MRS <$> reg 12 <*> choose 22 SPSR CPSR)
  
  , decoder ARM [ARM_EXT_V1]    0x03000000 0x0fe00000 (TST <$> reg 16 <*> arm_o) -- "tst%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01000000 0x0fe00010 (TST <$> reg 16 <*> arm_o) -- "tst%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01000010 0x0fe00090 (TST <$> reg 16 <*> arm_o) -- "tst%p%c\t%16-19R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x03200000 0x0fe00000 (TEQ <$> reg 16 <*> arm_o) -- "teq%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01200000 0x0fe00010 (TEQ <$> reg 16 <*> arm_o) -- "teq%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01200010 0x0fe00090 (TEQ <$> reg 16 <*> arm_o) -- "teq%p%c\t%16-19R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x03400000 0x0fe00000 (CMP <$> reg 16 <*> arm_o) -- "cmp%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01400000 0x0ff00010 (MRS <$> reg 12 <*> choose 22 SPSR CPSR) -- "mrs%c\t%12-15R, %22?SCPSR"},
  , decoder ARM [ARM_EXT_V1]    0x01400000 0x0fe00010 (CMP <$> reg 16 <*> arm_o) -- "cmp%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01400010 0x0fe00090 (CMP <$> reg 16 <*> arm_o) -- "cmp%p%c\t%16-19R, %o"},
  
  , decoder ARM [ARM_EXT_V1]    0x03600000 0x0fe00000 (CMN <$> reg 16 <*> arm_o) -- "cmn%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01600000 0x0fe00010 (CMN <$> reg 16 <*> arm_o) -- "cmn%p%c\t%16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01600010 0x0fe00090 (CMN <$> reg 16 <*> arm_o) -- "cmn%p%c\t%16-19R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x03800000 0x0fe00000 (orr <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "orr%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01800000 0x0fe00010 (orr <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "orr%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01800010 0x0fe00090 (orr <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "orr%20's%c\t%12-15R, %16-19R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x03a00000 0x0fef0000 (mov <$> bool 20 <*> reg 12 <*> arm_o)
  , decoder ARM [ARM_EXT_V1]    0x01a00000 0x0def0ff0 (mov <$> bool 20 <*> reg 12 <*> (Reg <$> reg 0))
  , decoder ARM [ARM_EXT_V1]    0x01a00000 0x0def0060 (lsl <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder ARM [ARM_EXT_V1]    0x01a00020 0x0def0060 (lsr <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder ARM [ARM_EXT_V1]    0x01a00040 0x0def0060 (asr <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder ARM [ARM_EXT_V1]    0x01a00060 0x0def0ff0 (rrx <$> bool 20 <*> reg 12 <*> reg 0)
  , decoder ARM [ARM_EXT_V1]    0x01a00060 0x0def0060 (ror <$> bool 20 <*> reg 12 <*> arm_q)
                                
  , decoder ARM [ARM_EXT_V1]    0x03c00000 0x0fe00000 (bic <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "bic%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01c00000 0x0fe00010 (bic <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "bic%20's%c\t%12-15r, %16-19r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01c00010 0x0fe00090 (bic <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o) -- "bic%20's%c\t%12-15R, %16-19R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x03e00000 0x0fe00000 (mvn <$> bool 20 <*> reg 12 <*> arm_o) -- "mvn%20's%c\t%12-15r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01e00000 0x0fe00010 (mvn <$> bool 20 <*> reg 12 <*> arm_o) -- "mvn%20's%c\t%12-15r, %o"},
  , decoder ARM [ARM_EXT_V1]    0x01e00010 0x0fe00090 (mvn <$> bool 20 <*> reg 12 <*> arm_o) -- "mvn%20's%c\t%12-15R, %o"},

  , decoder ARM [ARM_EXT_V1]    0x06000010 0x0e000010 (pure Undefined)

  
  , decoder ARM [ARM_EXT_V1]    0x049d0004 0x0fff0fff (POP <$> (Regs . pure <$> reg 12)) -- "pop%c\t{%12-15r}\t\t; (ldr%c %12-15r, %a)"},
  , decoder ARM [ARM_EXT_V1]    0x04500000 0x0c500000 (ldr Byte <$> arm_t <*> pure False <*> reg 12 <*> arm_a) -- "ldrb%t%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x04300000 0x0d700000 (LDRT <$> reg 12 <*> arm_a) -- "ldrt%c\t%12-15R, %a"},
  , decoder ARM [ARM_EXT_V1]    0x04100000 0x0c500000 (LDR  <$> reg 12 <*> arm_a) -- "ldr%c\t%12-15r, %a"},
  
  
  , decoder ARM [ARM_EXT_V1]    0x049d0004 0x0fff0fff (LDRH <$> reg 12 <*> arm_a)
  , decoder ARM [ARM_EXT_V1]    0x04100000 0x0c100000 (ldr <$> arm_bw 22 <*> arm_t <*> pure False <*> reg 12 <*> arm_a)

  , decoder ARM [ARM_EXT_V1]    0x092d0000 0x0fff0000 (PUSH <$> (Regs <$> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x08800000 0x0ff00000 (STM <$> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x08000000 0x0e100000 (stm <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x08bd0000 0x0fff0000 (POP <$> (Regs <$> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x08900000 0x0f900000 (LDM <$> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x08100000 0x0e100000 (ldm <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder ARM [ARM_EXT_V1]    0x0a000000 0x0e000000 (b <$> bool 24 <*> arm_b)
  , decoder ARM [ARM_EXT_V1]    0x0f000000 0x0f000000 (SVC <$> integral 0 23)

  , decoder ARM [ARM_EXT_V1]    0x00000000 0x00000000 (pure Undefined)
  ]


armDecode :: Word32 -> GeneralInstruction UAL
armDecode i = fromMaybe Undefined . fmap (decode ARM i) . find (decoderMatches ARM i) $ armDecoders

armDecodeDbg :: Word32 -> (GeneralInstruction UAL, (Word32, Word32))
armDecodeDbg i = case fromJust . find (decoderMatches ARM i) $ armDecoders of
                    GeneralDecoder a b c d -> (d i, (b, c))