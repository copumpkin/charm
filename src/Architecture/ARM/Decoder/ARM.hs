{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
module Architecture.ARM.Decoder.ARM where

import Prelude hiding (and)

import Architecture.ARM.Common
import Architecture.ARM.Instructions.UAL

import Data.Maybe
import Data.List hiding (and)
import Data.Int
import Data.Word
import Data.Bits hiding (bit)

import Text.Printf

import Control.Monad
import Control.Applicative

type D a = Word32 -> a

-- This all feels horribly repetitive...
instance Decoder Word32 Conditional where
  type Structure Word32 Conditional = ARMDecoder Word32 UALInstruction
  
  decoder archs value mask d = decoder archs value mask (Conditional <$> arm_c <*> d)

instance Decoder Word32 Unconditional where
  type Structure Word32 Unconditional = ARMDecoder Word32 UALInstruction

  decoder archs value mask d = decoder archs value mask (Unconditional <$> d)

instance Decoder Word32 UALInstruction where
  type Structure Word32 UALInstruction = ARMDecoder Word32 UALInstruction
  
  decoder = ARMDecoder


bitRange :: (Integral a, Bits a, Integral b) => Int -> Int -> a -> b
bitRange start end i = fromIntegral ((((fromIntegral i :: Integer) `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1)))

allSet :: Int -> Int -> Word32 -> Bool
allSet start end = (== (((2 :: Word32) `shiftL` (end - start)) - 1)) . bitRange start end

armDecodeAddress' :: D ARMOpMemory
armDecodeAddress' =
  do flag   <- bool 25
     pcrel  <- (&& not flag) <$> allSet 16 19
     post   <- not <$> bool 24
     neg    <- bool 23
     offset <- fromIntegral . (.&. 0xfff)
     base   <- reg 16
     let memReg = if neg then MemRegNeg else MemReg
         memRegPost = if neg then MemRegPostNeg else MemRegPost
         mem = if post then (\r o b -> memRegPost r o) else memReg
     if pcrel -- is this special case necessary?
       then
         mem PC (Imm offset) <$> bool 21  
       else
         if flag
           then
             mem base <$> flip armDecodeShift False <*> bool 21 
           else
             mem base (Imm offset) <$> bool 21

-- These things need to be tested!!
armDecodeAddress :: D ARMOpMemory
armDecodeAddress a | (a .&. 0xf0000) == 0xf0000 && not (bool 25 a) = 
                         let offset = a .&. 0xfff in
                           case bool 24 a of
                             True -> MemReg PC (Imm ((if not (bool 23 a) then negate else id) (fromIntegral offset))) (bool 21 a)
                             _    -> MemRegPost PC $ Imm (fromIntegral offset)
                   | otherwise = 
                         let baseReg = (toEnum (((fromIntegral a) `shiftR` 16 ) .&. 0xf)) in case bool 24 a of
                           False -> if not (bool 25 a) then
                                      let offset = a .&. 0xfff in
                                        MemRegPost baseReg $ Imm ((if not (bool 23 a) then negate else id) (fromIntegral offset))
                                      else (if not (bool 23 a) then MemRegPostNeg else MemRegPost) baseReg (armDecodeShift a False)
                           _     -> if not (bool 25 a) then
                                      let offset = a .&. 0xfff in
                                        MemReg baseReg (Imm (if not (bool 23 a) then -(fromIntegral offset) else fromIntegral offset)) (bool 21 a)
                                      else (if not (bool 23 a) then MemRegNeg else MemReg) baseReg (armDecodeShift a False) (bool 21 a)

armDecodeShift :: Word32 -> Bool -> ARMOpData
armDecodeShift i p =  if i .&. 0xff0 /= 0 then
                        if i .&. 0x10 == 0 then
                          let amount = (i .&. 0xf80) `shiftR` 7
                              shift = ((fromIntegral i) .&. 0x60) `shiftR` 5 in
                            if amount == 0 && shift == 3 then  RegShiftRRX (toEnum ((fromIntegral i) .&. 0xf)) 
                              else  RegShiftImm (toEnum shift) (fromIntegral amount) (toEnum ((fromIntegral i) .&. 0xf)) 
                          else  RegShiftReg (toEnum (((fromIntegral i) .&. 0x60) `shiftR` 5)) (toEnum ((((fromIntegral i) .&. 0xf00) `shiftR` 8))) (toEnum ((fromIntegral i) .&. 0xf)) 
                        else Reg (toEnum ((fromIntegral i) .&. 0xf))

arm_a :: D ARMOpMemory
arm_a = armDecodeAddress 

-- FIXME: wow, this is pretty ugly...
arm_s :: D ARMOpMemory
arm_s i | i .&. 0x4f0000 == 0x4f0000 = MemReg PC (Imm (fromIntegral $ (if i .&. 0x800000 == 0 then -1 else 1) * ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf))) False
        | i .&. 0x1000000 /= 0 = case i .&. 0x400000 of
            0x400000 -> MemReg (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) 
                            (Imm $ let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                              if (i .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset) 
                               ((i .&. 0x200000) /= 0)
            _        -> (if (i .&. 0x800000) == 0 then MemRegNeg else MemReg) (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf))
                            (Reg $ toEnum ((fromIntegral i) .&. 0xf)) ((i .&. 0x200000) /= 0)
        | otherwise = case i .&. 0x400000 of
            0x400000 -> MemReg (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) 
                            (Imm $ let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                              (if (i .&. 0x800000) == 0 then  -(fromIntegral offset) else fromIntegral offset))
                               False
            _        -> (if (i .&. 0x800000) == 0 then MemRegPostNeg else MemRegPost) (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) (Reg $ toEnum ((fromIntegral i) .&. 0xf))

arm_b :: D Int32
arm_b i = ((((fromIntegral i :: Int32) .&. 0xffffff) `xor` 0x800000) - 0x800000) * 4 + {-(fromIntegral $ pc s) + -} 8

arm_c :: D Condition
arm_c i = toEnum $ fromIntegral ((i `shiftR` 28) .&. 0xf)

arm_m :: D [ARMRegister]
arm_m i = catMaybes $ map (\x -> if i .&. (1 `shiftL` x) /= 0 then Just $ toEnum x else Nothing) [0..15]

arm_o :: D ARMOpData
arm_o i | i .&. 0x2000000 /= 0 = Imm . fromIntegral $ (i .&. 0xff) `rotateR` (((fromIntegral i) .&. 0xf00) `shiftR` 7)
        | otherwise = armDecodeShift i True

arm_p :: D Bool
arm_p i = i .&. 0xf000 == 0xf000

arm_t :: D Bool
arm_t i = i .&. 0x1200000 == 0x200000

arm_q :: D ARMOpData
arm_q i = armDecodeShift i False

arm_e :: D Word32
arm_e i = (i .&. 0xf) .|. ((i .&. 0xfff00) `shiftR` 4)

arm_B :: D Int32
arm_B = do x <- choose 23 0 0xff000000 -- negative bit
           y <- integral 0 23
           let offset = (x + y :: Int32) `shiftL` 2
           z <- choose 24 0 2 -- 2
           return (fromIntegral (offset + 8 + z)) -- FIXME: Do I want that +8 in there? Or should I deal with it later
            
         
-- FIXME: this is ugly
arm_C :: D String
arm_C i = '_' : (if i .&. 0x80000 /= 0 then "f" else "" ++ 
                 if i .&. 0x40000 /= 0 then "s" else "" ++
                 if i .&. 0x20000 /= 0 then "x" else "" ++
                 if i .&. 0x10000 /= 0 then "c" else "")

arm_U :: D ARMHint
arm_U i = case i .&. 0xf of
            0xf -> SY
            0x7 -> UN
            0xe -> ST
            0x6 -> UNST
            x   -> UK x

arm_P :: D ARMOpMemory
arm_P i = armDecodeAddress $ i .|. (1 `shiftL` 24)

reg :: Int -> D ARMRegister
reg start i = toEnum (bitRange start (start + 3)i)

integral :: (Integral a, Bits a) => Int -> Int -> D a
integral start end i = bitRange start end i

integral' :: (Integral a, Bits a) => Int -> Int -> D a
integral' start end i = (+1) . bitRange start end $ i

arm_X :: Int -> Int -> D Word32
arm_X start end i = (.&. 0xf) . bitRange start end $ i


arm_E :: D (Maybe (Word32, Word32))
arm_E i = let msb = (i .&. 0x1f0000) `shiftR` 16
              lsb = (i .&. 0xf80) `shiftR` 7
              width = msb - lsb + 1 in
            if width > 0 then
              Just (lsb, width) --"#" ++ (show lsb) ++ ", #" ++ (show width)
              else Nothing --"(invalid " ++ (show lsb) ++ ":" ++ (show msb) ++ ")"            

arm_V :: D Word32
arm_V i = (i .&. 0xf0000) `shiftR` 4 .|. (i .&. 0xfff)

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

direction :: Int -> D ARMDirection
direction n = choose n Decrement Increment

order :: Int -> D ARMOrder
order n = choose n After Before

choose :: Int -> a -> a -> Word32 -> a
choose n t f x = if not (bool n x) then t else f

bool20_reg12_reg16_o f = f <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o

pure32 :: a -> D a
pure32 = pure

armOpcodes :: [ARMDecoder Word32 UALInstruction]
armOpcodes = 
  [ decoder [ARM_EXT_V4T, ARM_EXT_V5] 0x012FFF10 0x0ffffff0 (BX <$> reg 0)
  , decoder [ARM_EXT_V2]    0x00000090 0x0fe000f0 (mul <$> bool 20 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V2]    0x00200090 0x0fe000f0 (mla <$> bool 20 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder [ARM_EXT_V2S]   0x01000090 0x0fb00ff0 (swp <$> bool 22 <*> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False))
  , decoder [ARM_EXT_V3M]   0x00800090 0x0fa000f0 (choose 22 umull smull <*> bool 20 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V3M]   0x00800090 0x0fa000f0 (choose 22 umlal smlal <*> bool 20 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)

  , decoder [ARM_EXT_V7]    0xf450f000 0xfd70f000 (PLI <$> arm_P)
  , decoder [ARM_EXT_V7]    0x0320f0f0 0x0ffffff0 (DBG <$> integral 0 3)
  , decoder [ARM_EXT_V7]    0xf57ff050 0x0ffffff0 (DMB <$> arm_U)
  , decoder [ARM_EXT_V7]    0xf57ff040 0x0ffffff0 (DSB <$> arm_U)
  , decoder [ARM_EXT_V7]    0xf57ff060 0x0ffffff0 (ISB <$> arm_U)

  , decoder [ARM_EXT_V6T2]  0x07c0001f 0x0fe0007f (BFC <$> reg 12 <*> arm_E)
  , decoder [ARM_EXT_V6T2]  0x07c00010 0x0fe00070 (BFI <$> reg 12 <*> reg 0 <*> arm_E)
  , decoder [ARM_EXT_V6T2]  0x00600090 0x0ff000f0 (MLS <$> reg 0 <*> reg 8 <*> reg 12)
  , decoder [ARM_EXT_V6T2]  0x006000b0 0x0f7000f0 (STRHT <$> reg 12 <*> arm_s) -- TODO: check me


  , decoder [ARM_EXT_V6T2]  0x00300090 0x0f3000f0 (pure32 Undefined :: Word32 -> UALInstruction)
  , decoder [ARM_EXT_V6T2]  0x00300090 0x0f300090 (ldr <$> arm_bh 5 <*> pure False <*> bool 6 <*> reg 12 <*> arm_s)

  , decoder [ARM_EXT_V6T2]  0x03000000 0x0ff00000 (MOVW <$> reg 12 <*> arm_V)
  , decoder [ARM_EXT_V6T2]  0x03400000 0x0ff00000 (MOVT <$> reg 12 <*> arm_V)
  , decoder [ARM_EXT_V6T2]  0x06ff0f30 0x0fff0ff0 (RBIT <$> reg 12 <*> reg 0)
  , decoder [ARM_EXT_V6T2]  0x07a00050 0x0fa00070 (choose 22 UBFX SBFX <*> reg 12 <*> reg 0 <*> integral 7 11 <*> integral' 16 20)

  , decoder [ARM_EXT_V6Z]   0x01600070 0x0ff000f0 (SMC <$> arm_e)

  , decoder [ARM_EXT_V6K]   0xf57ff01f 0xffffffff (pure32 CLREX) 
  , decoder [ARM_EXT_V6K]   0x01d00f9f 0x0ff00fff (LDREXB <$> reg 12 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False))
  , decoder [ARM_EXT_V6K]   0x01b00f9f 0x0ff00fff (do rt <- reg 12; rn <- reg 16; return (LDREXD rt (succ rt) (MemReg rn (Imm 0) False))) -- Doesn't really need to be this compliated. We could just have the second argument be implicit (but that makes things a little uglier to work with later)
  , decoder [ARM_EXT_V6K]   0x01f00f9f 0x0ff00fff (LDREXH <$> reg 12 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False))
  , decoder [ARM_EXT_V6K]   0x01c00f90 0x0ff00ff0 (STREXB <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False))
  , decoder [ARM_EXT_V6K]   0x01a00f90 0x0ff00ff0 (do rd <- reg 12; rn <- reg 16; rt <- reg 0; return (STREXD rd rt (succ rt) (MemReg rn (Imm 0) False))) -- As above
  , decoder [ARM_EXT_V6K]   0x01e00f90 0x0ff00ff0 (STREXH <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False))

  , decoder [ARM_EXT_V6K]   0x0320f001 0x0fffffff (pure32 YIELD)
  , decoder [ARM_EXT_V6K]   0x0320f002 0x0fffffff (pure32 WFE)
  , decoder [ARM_EXT_V6K]   0x0320f003 0x0fffffff (pure32 WFI)
  , decoder [ARM_EXT_V6K]   0x0320f004 0x0fffffff (pure32 SEV)
  , decoder [ARM_EXT_V6K]   0x0320f000 0x0fffff00 (pure32 NOP)
  
  , decoder [ARM_EXT_V6]    0xf1080000 0xfffffe3f (CPSIE <$> bool 8 <*> bool 7 <*> bool 6 <*> pure32 Nothing)
  , decoder [ARM_EXT_V6]    0xf10a0000 0xfffffe20 (CPSIE <$> bool 8 <*> bool 7 <*> bool 6 <*> (Just <$> integral 0 4))
  , decoder [ARM_EXT_V6]    0xf10C0000 0xfffffe3f (CPSID <$> bool 8 <*> bool 7 <*> bool 6 <*> pure32 Nothing)
  , decoder [ARM_EXT_V6]    0xf10e0000 0xfffffe20 (CPSID <$> bool 8 <*> bool 7 <*> bool 6 <*> (Just <$> integral 0 4))
  , decoder [ARM_EXT_V6]    0xf1000000 0xfff1fe20 (CPS <$> integral 0 4)

  , decoder [ARM_EXT_V6]    0x06800010 0x0ff00ff0 (PKHBT <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800010 0x0ff00070 (PKHBT <$> reg 12 <*> reg 16 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x06800050 0x0ff00ff0 (PKHTB <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ASR 32 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800050 0x0ff00070 (PKHTB <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x01900f9f 0x0ff00fff (LDREX  <$> reg 12 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False) )
  , decoder [ARM_EXT_V6]    0x06200f10 0x0ff00ff0 (reg12_reg16_reg0 $ QADD16)
  , decoder [ARM_EXT_V6]    0x06200f90 0x0ff00ff0 (reg12_reg16_reg0 $ QADD8)
  , decoder [ARM_EXT_V6]    0x06200f30 0x0ff00ff0 (reg12_reg16_reg0 $ QASX)
  , decoder [ARM_EXT_V6]    0x06200f70 0x0ff00ff0 (reg12_reg16_reg0 $ QSUB16)
  , decoder [ARM_EXT_V6]    0x06200ff0 0x0ff00ff0 (reg12_reg16_reg0 $ QSUB8)
  , decoder [ARM_EXT_V6]    0x06200f50 0x0ff00ff0 (reg12_reg16_reg0 $ QSAX)
  , decoder [ARM_EXT_V6]    0x06100f10 0x0ff00ff0 (reg12_reg16_reg0 $ SADD16)
  , decoder [ARM_EXT_V6]    0x06100f90 0x0ff00ff0 (reg12_reg16_reg0 $ SADD8)
  , decoder [ARM_EXT_V6]    0x06100f30 0x0ff00ff0 (reg12_reg16_reg0 $ SASX)
  , decoder [ARM_EXT_V6]    0x06300f10 0x0ff00ff0 (reg12_reg16_reg0 $ SHADD16)
  , decoder [ARM_EXT_V6]    0x06300f90 0x0ff00ff0 (reg12_reg16_reg0 $ SHADD8)
  , decoder [ARM_EXT_V6]    0x06300f30 0x0ff00ff0 (reg12_reg16_reg0 $ SHASX)
  , decoder [ARM_EXT_V6]    0x06300f70 0x0ff00ff0 (reg12_reg16_reg0 $ SHSUB16)
  , decoder [ARM_EXT_V6]    0x06300ff0 0x0ff00ff0 (reg12_reg16_reg0 $ SHSUB8)
  , decoder [ARM_EXT_V6]    0x06300f50 0x0ff00ff0 (reg12_reg16_reg0 $ SHSAX)
  , decoder [ARM_EXT_V6]    0x06100f70 0x0ff00ff0 (reg12_reg16_reg0 $ SSUB16)
  , decoder [ARM_EXT_V6]    0x06100ff0 0x0ff00ff0 (reg12_reg16_reg0 $ SSUB8)
  , decoder [ARM_EXT_V6]    0x06100f50 0x0ff00ff0 (reg12_reg16_reg0 $ SSAX)
  , decoder [ARM_EXT_V6]    0x06500f10 0x0ff00ff0 (reg12_reg16_reg0 $ UADD16)
  , decoder [ARM_EXT_V6]    0x06500f90 0x0ff00ff0 (reg12_reg16_reg0 $ UADD8)
  , decoder [ARM_EXT_V6]    0x06500f30 0x0ff00ff0 (reg12_reg16_reg0 $ UASX)
  , decoder [ARM_EXT_V6]    0x06700f10 0x0ff00ff0 (reg12_reg16_reg0 $ UHADD16)
  , decoder [ARM_EXT_V6]    0x06700f90 0x0ff00ff0 (reg12_reg16_reg0 $ UHADD8)
  , decoder [ARM_EXT_V6]    0x06700f30 0x0ff00ff0 (reg12_reg16_reg0 $ UHASX)
  , decoder [ARM_EXT_V6]    0x06700f70 0x0ff00ff0 (reg12_reg16_reg0 $ UHSUB16)
  , decoder [ARM_EXT_V6]    0x06700ff0 0x0ff00ff0 (reg12_reg16_reg0 $ UHSUB8)
  , decoder [ARM_EXT_V6]    0x06700f50 0x0ff00ff0 (reg12_reg16_reg0 $ UHSAX)
  , decoder [ARM_EXT_V6]    0x06600f10 0x0ff00ff0 (reg12_reg16_reg0 $ UQADD16)
  , decoder [ARM_EXT_V6]    0x06600f90 0x0ff00ff0 (reg12_reg16_reg0 $ UQADD8)
  , decoder [ARM_EXT_V6]    0x06600f30 0x0ff00ff0 (reg12_reg16_reg0 $ UQASX)
  , decoder [ARM_EXT_V6]    0x06600f70 0x0ff00ff0 (reg12_reg16_reg0 $ UQSUB16)
  , decoder [ARM_EXT_V6]    0x06600ff0 0x0ff00ff0 (reg12_reg16_reg0 $ UQSUB8)
  , decoder [ARM_EXT_V6]    0x06600f50 0x0ff00ff0 (reg12_reg16_reg0 $ UQSAX)
  , decoder [ARM_EXT_V6]    0x06500f70 0x0ff00ff0 (reg12_reg16_reg0 $ USUB16)
  , decoder [ARM_EXT_V6]    0x06500ff0 0x0ff00ff0 (reg12_reg16_reg0 $ USUB8)
  , decoder [ARM_EXT_V6]    0x06500f50 0x0ff00ff0 (reg12_reg16_reg0 $ USAX) 
  , decoder [ARM_EXT_V6]    0x06bf0f30 0x0fff0ff0 (REV     <$> reg 12 <*> reg 0)
  , decoder [ARM_EXT_V6]    0x06bf0fb0 0x0fff0ff0 (REV16   <$> reg 12 <*> reg 0)
  , decoder [ARM_EXT_V6]    0x06ff0fb0 0x0fff0ff0 (REVSH   <$> reg 12 <*> reg 0)
  , decoder [ARM_EXT_V6]    0xf8100a00 0xfe50ffff (rfe <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16)
  , decoder [ARM_EXT_V6]    0x06bf0070 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06bf0470 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06bf0870 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06bf0c70 0x0fff0ff0 (SXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x068f0070 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x068f0470 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x068f0870 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x068f0c70 0x0fff0ff0 (SXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06af0070 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06af0470 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06af0870 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06af0c70 0x0fff0ff0 (SXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ff0070 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ff0470 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ff0870 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ff0c70 0x0fff0ff0 (UXTH    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06cf0070 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06cf0470 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06cf0870 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06cf0c70 0x0fff0ff0 (UXTB16  <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ef0070 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ef0470 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ef0870 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06ef0c70 0x0fff0ff0 (UXTB    <$> reg 12 <*>            (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06b00070 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06b00470 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06b00870 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06b00c70 0x0ff00ff0 (SXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800070 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800470 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800870 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800c70 0x0ff00ff0 (SXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00070 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00470 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00870 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00c70 0x0ff00ff0 (SXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06f00070 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06f00470 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06f00870 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06f00c70 0x0ff00ff0 (UXTAH   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06c00070 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06c00470 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06c00870 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06c00c70 0x0ff00ff0 (UXTAB16 <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00070 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00470 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 8 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00870 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 16 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00c70 0x0ff00ff0 (UXTAB   <$> reg 12 <*> reg 16 <*> (RegShiftImm S_ROR 24 <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06800fb0 0x0ff00ff0 (reg12_reg16_reg0 $ SEL)
  , decoder [ARM_EXT_V6]    0xf1010000 0xfffffc00 (SETEND <$> choose 9 Big Little)
  , decoder [ARM_EXT_V6]    0x0700f010 0x0ff0f0d0 (smuad  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8) -- TODO: double check enum direction is correct for first arg
  , decoder [ARM_EXT_V6]    0x0700f050 0x0ff0f0d0 (smusd  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V6]    0x07000010 0x0ff000d0 (smlad  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder [ARM_EXT_V6]    0x07400010 0x0ff000d0 (smlald <$> enum . bit 5 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V6]    0x07000050 0x0ff000d0 (smlsd  <$> enum . bit 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12) 
  , decoder [ARM_EXT_V6]    0x07400050 0x0ff000d0 (smlsld <$> enum . bit 5 <*> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V6]    0x0750f010 0x0ff0f0d0 (smmul  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V6]    0x07500010 0x0ff000d0 (smmla  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder [ARM_EXT_V6]    0x075000d0 0x0ff000d0 (smmls  <$> bool 5 <*> reg 16 <*> reg 0 <*> reg 8 <*> reg 12)
  , decoder [ARM_EXT_V6]    0xf84d0500 0xfe5fffe0 (srs  <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> integral 0 4)
  , decoder [ARM_EXT_V6]    0x06a00010 0x0fe00ff0 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00010 0x0fe00070 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00050 0x0fe00070 (SSAT   <$> reg 12 <*> integral' 16 20 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x06a00f30 0x0ff00ff0 (SSAT16 <$> reg 12 <*> integral' 16 19 <*> reg 0)
  , decoder [ARM_EXT_V6]    0x01800f90 0x0ff00ff0 (STREX  <$> reg 12 <*> reg 0 <*> (MemReg <$> reg 16 <*> pure32 (Imm 0) <*> pure32 False) )
  , decoder [ARM_EXT_V6]    0x00400090 0x0ff000f0 (UMAAL  <$> reg 12 <*> reg 16 <*> reg 0 <*> reg 8)
  , decoder [ARM_EXT_V6]    0x0780f010 0x0ff0f0f0 (reg16_reg0_reg8 $ USAD8 )
  , decoder [ARM_EXT_V6]    0x07800010 0x0ff000f0 (reg16_reg0_reg8_reg12 $ USADA8)
  , decoder [ARM_EXT_V6]    0x06e00010 0x0fe00ff0 (USAT   <$> reg 12 <*> integral 16 20 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00010 0x0fe00070 (USAT   <$> reg 12 <*> integral 16 20 <*> (RegShiftImm S_LSL <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00050 0x0fe00070 (USAT   <$> reg 12 <*> integral 16 20 <*> (RegShiftImm S_ASR <$> integral 7 11 <*> reg 0))
  , decoder [ARM_EXT_V6]    0x06e00f30 0x0ff00ff0 (USAT16 <$> reg 12 <*> integral 16 19 <*> reg 0)
 
  , decoder [ARM_EXT_V5J]   0x012fff20 0x0ffffff0 (BXJ <$> reg 0)
 
  , decoder [ARM_EXT_V5]    0xe1200070 0xfff000f0 (BKPT <$> ((\x y -> x `shiftL` 4 .|. y) <$> integral 8 19 <*> integral 0 3)) 
  , decoder [ARM_EXT_V5]    0xfa000000 0xfe000000 (BLXUC <$> arm_B)
  , decoder [ARM_EXT_V5]    0x012fff30 0x0ffffff0 (BLX <$> reg 0)
  , decoder [ARM_EXT_V5]    0x016f0f10 0x0fff0ff0 (CLZ <$> reg 12 <*> reg 0)
  
  , decoder [ARM_EXT_V5E]   0x000000d0 0x0e1000f0 (LDRD <$> reg 12 <*> arm_s)
  , decoder [ARM_EXT_V5E]   0x000000f0 0x0e1000f0 (STRD <$> reg 12 <*> arm_s)
  , decoder [ARM_EXT_V5E]   0xf450f000 0xfc70f000 (PLD <$> arm_a)
  , decoder [ARM_EXT_V5ExP] 0x01000080 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLABB)
  , decoder [ARM_EXT_V5ExP] 0x010000a0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLATB)
  , decoder [ARM_EXT_V5ExP] 0x010000c0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLABT)
  , decoder [ARM_EXT_V5ExP] 0x010000e0 0x0ff000f0 (reg16_reg0_reg8_reg12 $ SMLATT)
  
  , decoder [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLAWB)
  , decoder [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLAWT)
  
  , decoder [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALBB)
  , decoder [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALTB)
  , decoder [ARM_EXT_V5ExP] 0x014000c0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALBT)
  , decoder [ARM_EXT_V5ExP] 0x014000e0 0x0ff000f0 (reg12_reg16_reg0_reg8 $ SMLALTT)
  
  , decoder [ARM_EXT_V5ExP] 0x01600080 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULBB)
  , decoder [ARM_EXT_V5ExP] 0x016000a0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULTB)
  , decoder [ARM_EXT_V5ExP] 0x016000c0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULBT)
  , decoder [ARM_EXT_V5ExP] 0x016000e0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULTT)
  
  , decoder [ARM_EXT_V5ExP] 0x012000a0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULWB)
  , decoder [ARM_EXT_V5ExP] 0x012000e0 0x0ff0f0f0 (reg16_reg0_reg8 $ SMULWT)
  
  , decoder [ARM_EXT_V5ExP] 0x01000050 0x0ff00ff0 (reg12_reg0_reg16 $ QADD)
  , decoder [ARM_EXT_V5ExP] 0x01400050 0x0ff00ff0 (reg12_reg0_reg16 $ QDADD)
  , decoder [ARM_EXT_V5ExP] 0x01200050 0x0ff00ff0 (reg12_reg0_reg16 $ QSUB)
  , decoder [ARM_EXT_V5ExP] 0x01600050 0x0ff00ff0 (reg12_reg0_reg16 $ QDSUB)
  
  -- This matches arm-dis.c, but is it right? It doesn't match encoding A1 or A2 in the reference
  , decoder [ARM_EXT_V1] 0x052d0004 0x0fff0fff (PUSH <$> Regs <$> (pure <$> reg 12)) 
  
  , decoder [ARM_EXT_V1] 0x04400000 0x0e500000 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder [ARM_EXT_V1] 0x04000000 0x0e500000 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
  , decoder [ARM_EXT_V1] 0x06400000 0x0e500ff0 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder [ARM_EXT_V1] 0x06000000 0x0e500ff0 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
  , decoder [ARM_EXT_V1] 0x04400000 0x0c500010 (str Byte <$> arm_t <*> reg 12 <*> arm_a) -- "strb%t%c\t%12-15R, %a"},
  , decoder [ARM_EXT_V1] 0x04000000 0x0c500010 (str Word <$> arm_t <*> reg 12 <*> arm_a) -- "str%t%c\t%12-15r, %a"},
  
  , decoder [ARM_EXT_V1] 0x04400000 0x0e500000 (STRB <$> reg 12 <*> arm_a) -- "strb%c\t%12-15R, %a"},
  , decoder [ARM_EXT_V1] 0x06400000 0x0e500010 (STRB <$> reg 12 <*> arm_a) -- "strb%c\t%12-15R, %a"},
  , decoder [ARM_EXT_V1] 0x004000b0 0x0e5000f0 (STRH <$> reg 12 <*> arm_a) -- "strh%c\t%12-15R, %s"},
  , decoder [ARM_EXT_V1] 0x000000b0 0x0e500ff0 (STRH <$> reg 12 <*> arm_a) -- "strh%c\t%12-15R, %s"},
  
  , decoder [ARM_EXT_V1] 0x00500090 0x0e5000f0 (pure32 Undefined)
  , decoder [ARM_EXT_V1] 0x00500090 0x0e500090 (ldr <$> arm_bh 5 <*> pure32 False <*> bool 6 <*> reg 12 <*> arm_s) -- "ldr%6's%5?hb%c\t%12-15R, %s"},
  , decoder [ARM_EXT_V1] 0x00100090 0x0e500ff0 (pure32 Undefined)
  , decoder [ARM_EXT_V1] 0x00100090 0x0e500f90 (ldr <$> arm_bh 5 <*> pure32 False <*> bool 6 <*> reg 12 <*> arm_s) -- "ldr%6's%5?hb%c\t%12-15R, %s"},
  
  , decoder [ARM_EXT_V1]    0x00000000 0x0de00000 (bool20_reg12_reg16_o $ and)
  --{ARM_EXT_V1, 0x02000000, 0x0fe00000, "and%20's%c\t%12-15r, %16-19r, %o"},
  --{ARM_EXT_V1, 0x00000000, 0x0fe00010, "and%20's%c\t%12-15r, %16-19r, %o"},
  --{ARM_EXT_V1, 0x00000010, 0x0fe00090, "and%20's%c\t%12-15R, %16-19R, %o"}, 

  , decoder [ARM_EXT_V1]    0x00200000 0x0de00000 (bool20_reg12_reg16_o $ eor)
  -- {ARM_EXT_V1, 0x02200000, 0x0fe00000, "eor%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00200000, 0x0fe00010, "eor%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00200010, 0x0fe00090, "eor%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00400000 0x0de00000 (bool20_reg12_reg16_o $ sub)
  -- {ARM_EXT_V1, 0x02400000, 0x0fe00000, "sub%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00400000, 0x0fe00010, "sub%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00400010, 0x0fe00090, "sub%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00600000 0x0de00000 (bool20_reg12_reg16_o $ rsb)
  -- {ARM_EXT_V1, 0x02600000, 0x0fe00000, "rsb%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00600000, 0x0fe00010, "rsb%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00600010, 0x0fe00090, "rsb%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00800000 0x0de00000 (bool20_reg12_reg16_o $ add)
  -- {ARM_EXT_V1, 0x02800000, 0x0fe00000, "add%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00800000, 0x0fe00010, "add%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00800010, 0x0fe00090, "add%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00a00000 0x0de00000 (bool20_reg12_reg16_o $ adc)
  -- {ARM_EXT_V1, 0x02a00000, 0x0fe00000, "adc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00a00000, 0x0fe00010, "adc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00a00010, 0x0fe00090, "adc%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00c00000 0x0de00000 (bool20_reg12_reg16_o $ sbc)
  -- {ARM_EXT_V1, 0x02c00000, 0x0fe00000, "sbc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00c00000, 0x0fe00010, "sbc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00c00010, 0x0fe00090, "sbc%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x00e00000 0x0de00000 (bool20_reg12_reg16_o $ rsc)
  -- {ARM_EXT_V1, 0x02e00000, 0x0fe00000, "rsc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00e00000, 0x0fe00010, "rsc%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x00e00010, 0x0fe00090, "rsc%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V3]    0x0120f000 0x0db0f000 (MSR <$> bool 18 <*> bool 19 <*> arm_o)
  , decoder [ARM_EXT_V3]    0x010f0000 0x0fbf0fff (MRS <$> reg 12 <*> choose 22 SPSR CPSR)
  
  , decoder [ARM_EXT_V1]    0x01000000 0x0de00000 (TST <$> reg 16 <*> arm_o)
  -- {ARM_EXT_V1, 0x03000000, 0x0fe00000, "tst%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01000000, 0x0fe00010, "tst%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01000010, 0x0fe00090, "tst%p%c\t%16-19R, %o"},
  
  , decoder [ARM_EXT_V1]    0x01200000 0x0de00000 (TEQ <$> reg 16 <*> arm_o)
  -- {ARM_EXT_V1, 0x03200000, 0x0fe00000, "teq%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01200000, 0x0fe00010, "teq%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01200010, 0x0fe00090, "teq%p%c\t%16-19R, %o"},
  
  , decoder [ARM_EXT_V1]    0x01400000 0x0de00000 (CMP <$> reg 16 <*> arm_o) 
  -- {ARM_EXT_V1, 0x03400000, 0x0fe00000, "cmp%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V3, 0x01400000, 0x0ff00010, "mrs%c\t%12-15R, %22?SCPSR"},
  -- {ARM_EXT_V1, 0x01400000, 0x0fe00010, "cmp%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01400010, 0x0fe00090, "cmp%p%c\t%16-19R, %o"},
  
  , decoder [ARM_EXT_V1]    0x01600000 0x0de00000 (CMN <$> reg 16 <*> arm_o)
  -- {ARM_EXT_V1, 0x03600000, 0x0fe00000, "cmn%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01600000, 0x0fe00010, "cmn%p%c\t%16-19r, %o"},
  -- {ARM_EXT_V1, 0x01600010, 0x0fe00090, "cmn%p%c\t%16-19R, %o"},
  
  , decoder [ARM_EXT_V1]    0x01800000 0x0de00000 (orr <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o)
  -- {ARM_EXT_V1, 0x03800000, 0x0fe00000, "orr%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x01800000, 0x0fe00010, "orr%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x01800010, 0x0fe00090, "orr%20's%c\t%12-15R, %16-19R, %o"},

  , decoder [ARM_EXT_V1]    0x03a00000 0x0fef0000 (mov <$> bool 20 <*> reg 12 <*> arm_o)
  , decoder [ARM_EXT_V1]    0x01a00000 0x0def0ff0 (mov <$> bool 20 <*> reg 12 <*> (Reg <$> reg 0))
  , decoder [ARM_EXT_V1]    0x01a00000 0x0def0060 (lsl <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder [ARM_EXT_V1]    0x01a00020 0x0def0060 (lsr <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder [ARM_EXT_V1]    0x01a00040 0x0def0060 (asr <$> bool 20 <*> reg 12 <*> arm_q)
  , decoder [ARM_EXT_V1]    0x01a00060 0x0def0ff0 (rrx <$> bool 20 <*> reg 12 <*> reg 0)
  , decoder [ARM_EXT_V1]    0x01a00060 0x0def0060 (ror <$> bool 20 <*> reg 12 <*> arm_q)
                                
  , decoder [ARM_EXT_V1]    0x01c00000 0x0de00000 (bic <$> bool 20 <*> reg 12 <*> reg 16 <*> arm_o)
  -- {ARM_EXT_V1, 0x03c00000, 0x0fe00000, "bic%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x01c00000, 0x0fe00010, "bic%20's%c\t%12-15r, %16-19r, %o"},
  -- {ARM_EXT_V1, 0x01c00010, 0x0fe00090, "bic%20's%c\t%12-15R, %16-19R, %o"},
  
  , decoder [ARM_EXT_V1]    0x01e00000 0x0de00000 (mvn <$> bool 20 <*> reg 12 <*> arm_o)
  -- {ARM_EXT_V1, 0x03e00000, 0x0fe00000, "mvn%20's%c\t%12-15r, %o"},
  -- {ARM_EXT_V1, 0x01e00000, 0x0fe00010, "mvn%20's%c\t%12-15r, %o"},
  -- {ARM_EXT_V1, 0x01e00010, 0x0fe00090, "mvn%20's%c\t%12-15R, %o"},
  

  , decoder [ARM_EXT_V1]    0x06000010 0x0e000010 (pure32 Undefined)
  
  -- {ARM_EXT_V1, 0x049d0004, 0x0fff0fff, "pop%c\t{%12-15r}\t\t; (ldr%c %12-15r, %a)"},
  -- 
  -- {ARM_EXT_V1, 0x04500000, 0x0c500000, "ldrb%t%c\t%12-15R, %a"},
  -- 
  -- {ARM_EXT_V1, 0x04300000, 0x0d700000, "ldrt%c\t%12-15R, %a"},
  -- {ARM_EXT_V1, 0x04100000, 0x0c500000, "ldr%c\t%12-15r, %a"},
  
  
  , decoder [ARM_EXT_V1]    0x049d0004 0x0fff0fff (LDRH <$> reg 12 <*> arm_a)
  , decoder [ARM_EXT_V1]    0x04100000 0x0c100000 (ldr <$> arm_bw 22 <*> arm_t <*> pure32 False <*> reg 12 <*> arm_a)

  , decoder [ARM_EXT_V1]    0x092d0000 0x0fff0000 (PUSH <$> (Regs <$> arm_m))
  , decoder [ARM_EXT_V1]    0x08800000 0x0ff00000 (STM <$> bool 21 <*> reg 16 <*> (RegsCaret <$> arm_m))
  , decoder [ARM_EXT_V1]    0x08000000 0x0e100000 (stm <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder [ARM_EXT_V1]    0x08bd0000 0x0fff0000 (POP <$> (Regs <$> arm_m))
  , decoder [ARM_EXT_V1]    0x08900000 0x0f900000 (LDM <$> bool 21 <*> reg 16 <*> (RegsCaret <$> arm_m))
  , decoder [ARM_EXT_V1]    0x08100000 0x0e100000 (ldm <$> direction 23 <*> order 24 <*> bool 21 <*> reg 16 <*> (choose 22 Regs RegsCaret <*> arm_m))
  , decoder [ARM_EXT_V1]    0x0a000000 0x0e000000 (b <$> bool 24 <*> arm_b)
  , decoder [ARM_EXT_V1]    0x0f000000 0x0f000000 (SVC <$> integral 0 23)

  , decoder [ARM_EXT_V1]    0x00000000 0x00000000 (pure32 Undefined)
  ]

armDecode :: Word32 -> UALInstruction
armDecode i = fromMaybe Undefined . fmap (armDecodeOp i) . find (armOpcodeMatches i) $ armOpcodes

armDecodeDbg :: Word32 -> (UALInstruction, Word32, Word32)
armDecodeDbg i = case fromJust . find (armOpcodeMatches i) $ armOpcodes of
                    ARMDecoder a b c d -> (armDecode i, b, c)