module Architecture.ARM.ARM where

import Architecture.ARM.Common
import Architecture.ARM.Instructions

import Data.Maybe
import Data.List
import Data.Int
import Data.Word
import Data.Bits hiding (bit)

import Text.Printf

import Control.Monad
import Control.Applicative


data ARMOpcode32 = ARMOpcode32 { opcode32_arch :: [ARMArch]
                               ,  opcode32_value :: Word32
                               ,  opcode32_mask :: Word32
                               ,  opcode32_decoder :: ARMDecoder ARMInstruction
                               }


bitRange :: (Integral a, Bits a) => Int -> Int -> a -> a
bitRange start end i = ((i `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1))

type ARMDecoder a = Word32 -> a

armDecodeAddress :: ARMDecoder ARMOpMemory
armDecodeAddress a | (a .&. 0xf0000) == 0xf0000 && (a .&. 0x2000000) == 0 = 
                         let offset = a .&. 0xfff in
                           case a .&. 0x1000000 /= 0 of
                             True -> MemReg PC (Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)) ((a .&. 0x200000) /= 0)
                             _    -> MemRegPost PC $ Imm (fromIntegral offset)
                   | otherwise = 
                         let baseReg = (toEnum (((fromIntegral a) `shiftR` 16 ) .&. 0xf)) in case a .&. 0x1000000 /= 0 of
                           False -> if (a .&. 0x2000000) == 0 then
                                      let offset = a .&. 0xfff in
                                        if offset /= 0 then
                                          MemRegPost baseReg $ Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)
                                          else MemRegPost baseReg $ Imm 0
                                      else (if (a .&. 0x800000) == 0 then MemRegPostNeg else MemRegPost) baseReg (armDecodeShift a False)
                           _     -> if (a .&. 0x2000000) == 0 then
                                      let offset = a .&. 0xfff in
                                        MemReg baseReg (Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)) ((a .&. 0x200000) /= 0)
                                      else (if (a .&. 0x800000) == 0 then MemRegNeg else MemReg) baseReg (armDecodeShift a False) ((a .&. 0x200000) /= 0)

armDecodeShift :: Word32 -> Bool -> ARMOpData
armDecodeShift i p =  if i .&. 0xff0 /= 0 then
                        if i .&. 0x10 == 0 then
                          let amount = (i .&. 0xf80) `shiftR` 7
                              shift = ((fromIntegral i) .&. 0x60) `shiftR` 5 in
                            if amount == 0 && shift == 3 then  RegShiftRRX (toEnum ((fromIntegral i) .&. 0xf)) 
                              else  RegShiftImm (toEnum shift) (fromIntegral amount) (toEnum ((fromIntegral i) .&. 0xf)) 
                          else  RegShiftImm (toEnum (((fromIntegral i) .&. 0x60) `shiftR` 5)) (toEnum (((fromIntegral i) .&. 0xf00 `shiftR` 8))) (toEnum ((fromIntegral i) .&. 0xf)) 
                        else Reg (toEnum ((fromIntegral i) .&. 0xf))

arm_const :: String -> ARMDecoder String
arm_const x i = x

arm_constint :: Int -> ARMDecoder String
arm_constint x i = show x

arm_a :: ARMDecoder ARMOpMemory
arm_a = armDecodeAddress 

-- FIXME: wow, this is pretty ugly...
arm_s :: ARMDecoder ARMOpMemory
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

arm_b :: ARMDecoder Int32
arm_b i = ((((fromIntegral i :: Int32) .&. 0xffffff) `xor` 0x800000) - 0x800000) * 4 + {-(fromIntegral $ pc s) + -} 8

arm_c :: ARMDecoder ARMCondition
arm_c i = toEnum $ fromIntegral ((i `shiftR` 28) .&. 0xf)

arm_m :: ARMDecoder ARMOpMultiple
arm_m i = Regs . catMaybes $ map (\x -> if i .&. (1 `shiftL` x) /= 0 then Just $ toEnum x else Nothing) [0..15]

arm_o :: ARMDecoder ARMOpData
arm_o i | i .&. 0x2000000 /= 0 = Imm . fromIntegral $ (i .&. 0xff) `rotateR` (((fromIntegral i) .&. 0xf00) `shiftR` 7)
        | otherwise = armDecodeShift i True

arm_p :: ARMDecoder Bool
arm_p i = i .&. 0xf000 == 0xf000

arm_t :: ARMDecoder Bool
arm_t i = i .&. 0x1200000 == 0x200000

arm_q :: ARMDecoder ARMOpData
arm_q i = armDecodeShift i False

arm_e :: ARMDecoder Word32
arm_e i = (i .&. 0xf) .|. ((i .&. 0xfff00) `shiftR` 4)

arm_B :: ARMDecoder Int32
arm_B i = let offset = ((if i .&. 0x800000 /= 0 then 0xff else 0) + (i .&. 0xffffff)) `shiftL` 2 
              address = offset + {-(pc s) + -} 8 + (if i .&. 0x1000000 /= 0 then 2 else 0) in
                fromIntegral address
              
-- FIXME: this is ugly
arm_C :: ARMDecoder String
arm_C i = '_' : (if i .&. 0x80000 /= 0 then "f" else "" ++ 
                 if i .&. 0x40000 /= 0 then "s" else "" ++
                 if i .&. 0x20000 /= 0 then "x" else "" ++
                 if i .&. 0x10000 /= 0 then "c" else "")

arm_U :: ARMDecoder ARMHint
arm_U i = case i .&. 0xf of
            0xf -> SY
            0x7 -> UN
            0xe -> ST
            0x6 -> UNST
            x   -> UK x

arm_P :: ARMDecoder ARMOpMemory
arm_P i = armDecodeAddress $ i .|. (1 `shiftL` 24)

reg :: Int -> Int -> ARMDecoder ARMRegister
reg start end i = toEnum (bitRange start end $ fromIntegral i)

arm_d :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_d start end i = bitRange start end $ fromIntegral i

arm_W :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_W start end i = (+1) . bitRange start end $ fromIntegral i

arm_x :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_x = arm_d

arm_X :: Int -> Int -> ARMDecoder Word32
arm_X start end i = (.&. 0xf) . bitRange start end $ i

arm_arr :: Int -> Int -> [a] -> ARMDecoder a
arm_arr start end c i = c !! (fromIntegral $ bitRange start end i)

arm_E :: ARMDecoder (Maybe (Word32, Word32))
arm_E i = let msb = (i .&. 0x1f0000) `shiftR` 16
              lsb = (i .&. 0xf80) `shiftR` 7
              width = msb - lsb + 1 in
            if width > 0 then
              Just (lsb, width) --"#" ++ (show lsb) ++ ", #" ++ (show width)
              else Nothing --"(invalid " ++ (show lsb) ++ ":" ++ (show msb) ++ ")"            

arm_V :: ARMDecoder Word32
arm_V i = (i .&. 0xf0000) `shiftR` 4 .|. (i .&. 0xfff)

{-
arm_square :: ARMDecoder -> ARMDecoder
arm_square d = ((("[" ++) . (++ "]")) .) . d

arm_curly :: ARMDecoder -> ARMDecoder
arm_curly d = ((("{" ++) . (++ "}")) .) . d

-}

bit b i = bitRange b b i

bool b s = bit b s == 1

arm_uncond = liftM  ARMUnconditionalInstruction
arm_cond   = liftM2 ARMConditionalInstruction arm_c

arm_bw bit i = if bitRange bit bit i == 1 then Byte else Word

reg12'15_reg0'3_reg16'19 f = f <$> reg 12 15 <*> reg 0 3 <*> reg 16 19

reg12'15_reg16'19_reg0'3 f = f <$> reg 12 15 <*> reg 16 19 <*> reg 0 3
reg12'15_reg16'19_reg0'3_reg8'11 f = reg12'15_reg16'19_reg0'3 f <*> reg 8 11
reg16'19_reg0'3_reg8'11 f = f <$> reg 16 19 <*> reg 0 3 <*> reg 8 11
reg16'19_reg0'3_reg8'11_reg12'15 f = reg16'19_reg0'3_reg8'11 f <*> reg 12 15

bool20_reg12'15_reg16'19_o f = f <$> bool 20 <*> reg 12 15 <*> reg 16 19 <*> arm_o

armOpcodes = 
  [ --ARMOpcode32 [ARM_EXT_V1] 0xe1a00000 0xffffffff [arm_const "nop"]
    ARMOpcode32 [ARM_EXT_V4T, ARM_EXT_V5] 0x012FFF10 0x0ffffff0 (arm_cond $ BX <$> (reg 0 3))
  , ARMOpcode32 [ARM_EXT_V2]    0x00000090 0x0fe000f0 (arm_cond $ MUL <$> bool 20 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  , ARMOpcode32 [ARM_EXT_V2]    0x00200090 0x0fe000f0 (arm_cond $ MLA <$> bool 20 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11 <*> reg 12 15)
  --, ARMOpcode32 [ARM_EXT_V2S] 0x01000090 0x0fb00ff0 [arm_const "swp", arm_char1 22 22 'b', arm_c, reg 12 15, reg 0 3, arm_square (reg 16 19)]
  , ARMOpcode32 [ARM_EXT_V3M]   0x00800090 0x0fa000f0 (arm_cond $ (arm_arr 22 22 [SMULL, UMULL]) <*> bool 20 <*> reg 12 15 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  --,ARMOpcode32 [ARM_EXT_V3M]  0x00800090 0x0fa000f0 (arm_cond $ (arm_arr 22 22 [SMLAL, UMLAL]) <*> (bool 20) <*> (reg 12 15) <*> (reg 16 19) <*> (reg 0 3) <*> (reg 8 11))
  , ARMOpcode32 [ARM_EXT_V7]    0xf450f000 0xfd70f000 (arm_cond $ PLI <$> arm_P)
  , ARMOpcode32 [ARM_EXT_V7]    0x0320f0f0 0x0ffffff0 (arm_cond $ DBG <$> arm_d 0 3)
  , ARMOpcode32 [ARM_EXT_V7]    0xf57ff050 0x0ffffff0 (arm_cond $ DMB <$> arm_U)
  , ARMOpcode32 [ARM_EXT_V7]    0xf57ff040 0x0ffffff0 (arm_cond $ DSB <$> arm_U)
  , ARMOpcode32 [ARM_EXT_V7]    0xf57ff060 0x0ffffff0 (arm_cond $ ISB <$> arm_U)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x07c0001f 0x0fe0007f (arm_cond $ BFC <$> reg 12 15 <*> arm_E)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x07c00010 0x0fe00070 (arm_cond $ BFI <$> reg 12 15 <*> reg 0 3 <*> arm_E)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x00600090 0x0ff000f0 (arm_cond $ MLS <$> reg 0 3 <*> reg 8 11 <*> reg 12 15)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x006000b0 0x0f7000f0 (arm_cond $ STR HalfWord True False <$> reg 12 15 <*> arm_s) -- TODO: check me
  , ARMOpcode32 [ARM_EXT_V6T2]  0x00300090 0x0f300090 (arm_cond $ LDR <$> arm_arr 5 5 [HalfWord, Byte] <*> const False <*> bool 6 <*> reg 12 15 <*> arm_s)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x03000000 0x0ff00000 (arm_cond $ MOVW <$> reg 12 15 <*> arm_V)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x03400000 0x0ff00000 (arm_cond $ MOVT <$> reg 12 15 <*> arm_V)
  , ARMOpcode32 [ARM_EXT_V6T2]  0x06ff0f30 0x0fff0ff0 (arm_cond $ RBIT <$> reg 12 15 <*> reg 0 3)
--, ARMOpcode32 [ARM_EXT_V6T2]  0x07a00050 0x0fa00070 [arm_arr 22 22 ['u', 's'], arm_const "bfx", arm_c, reg 12 15, reg 0 3, arm_d 7 11, arm_W 16 20]
--, ARMOpcode32 [ARM_EXT_V6Z]   0x01600070 0x0ff000f0 [arm_const "smc", arm_c, arm_e]
--, ARMOpcode32 [ARM_EXT_V6K]   0xf57ff01f 0xffffffff [arm_const "clrex"]
--, ARMOpcode32 [ARM_EXT_V6K]   0x01d00f9f 0x0ff00fff [arm_const "ldrexb", arm_c, reg 12 15, arm_square (reg 16 19)]
--, ARMOpcode32 [ARM_EXT_V6K]   0x01b00f9f 0x0ff00fff [arm_const "ldrexd", arm_c, reg 12 15, arm_square (reg 16 19)] 
--, ARMOpcode32 [ARM_EXT_V6K]   0x01f00f9f 0x0ff00fff [arm_const "ldrexh", arm_c, reg 12 15, arm_square (reg 16 19)] 
--, ARMOpcode32 [ARM_EXT_V6K]   0x01c00f90 0x0ff00ff0 [arm_const "strexb", arm_c, reg 12 15, reg 0 3, arm_square (reg 16 19)]
--, ARMOpcode32 [ARM_EXT_V6K]   0x01a00f90 0x0ff00ff0 [arm_const "strexd", arm_c, reg 12 15, reg 0 3, arm_square (reg 16 19)] 
--, ARMOpcode32 [ARM_EXT_V6K]   0x01e00f90 0x0ff00ff0 [arm_const "strexh", arm_c, reg 12 15, reg 0 3, arm_square (reg 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6K]   0x0320f001 0x0fffffff (arm_cond $ pure YIELD)
  , ARMOpcode32 [ARM_EXT_V6K]   0x0320f002 0x0fffffff (arm_cond $ pure WFE)
  , ARMOpcode32 [ARM_EXT_V6K]   0x0320f003 0x0fffffff (arm_cond $ pure WFI)
  , ARMOpcode32 [ARM_EXT_V6K]   0x0320f004 0x0fffffff (arm_cond $ pure SEV)
--, ARMOpcode32 [ARM_EXT_V6K]   0x0320f000 0x0fffff00 [arm_const "nop", arm_c, arm_curly (arm_d 0 7)]
--, ARMOpcode32 [ARM_EXT_V6]    0xf1080000 0xfffffe3f [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f']
--, ARMOpcode32 [ARM_EXT_V6]    0xf10a0000 0xfffffe20 [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
--, ARMOpcode32 [ARM_EXT_V6]    0xf10C0000 0xfffffe3f [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f'] 
--, ARMOpcode32 [ARM_EXT_V6]    0xf10e0000 0xfffffe20 [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
--, ARMOpcode32 [ARM_EXT_V6]    0xf1000000 0xfff1fe20 [arm_const "cps", arm_d 0 4]
  , ARMOpcode32 [ARM_EXT_V6]    0x06800010 0x0ff00ff0 (arm_cond $ PKHBT <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800010 0x0ff00070 (arm_cond $ PKHBT <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_LSL <$> arm_d 7 11 <*> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800050 0x0ff00ff0 (arm_cond $ PKHTB <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ASR 32 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800050 0x0ff00070 (arm_cond $ PKHTB <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ASR <$> arm_d 7 11 <*> reg 0 3))
--, ARMOpcode32 [ARM_EXT_V6]    0x01900f9f 0x0ff00fff (arm_cond . reg12'15_reg16'19_reg0'3 $ LDREX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QADD8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QADDSUBX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QSUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QSUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06200f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ QSUBADDX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06100f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06100f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SADD8)
--, ARMOpcode32 [ARM_EXT_V6]    0x06100f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SADDADDX) -- http://sourceware.org/bugzilla/show_bug.cgi?id=6773
  , ARMOpcode32 [ARM_EXT_V6]    0x06300f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06300f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHADD8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06300f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHADDSUBX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06300f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHSUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06300ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHSUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06300f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SHSUBADDX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06100f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SSUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06100ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SSUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06100f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SSUBADDX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UADD8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UADDSUBX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHADD8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHADDSUBX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHSUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHSUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06700f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UHSUBADDX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600f10 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQADD16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600f90 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQADD8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600f30 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQADDSUBX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQSUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQSUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06600f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ UQSUBADDX)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500f70 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ USUB16)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500ff0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ USUB8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06500f50 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ USUBADDX) 
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0f30 0x0fff0ff0 (arm_cond $ REV       <$> reg 12 15 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0fb0 0x0fff0ff0 (arm_cond $ REV16     <$> reg 12 15 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V6]    0x06ff0fb0 0x0fff0ff0 (arm_cond $ REVSH     <$> reg 12 15 <*> reg 0 3)
--, ARMOpcode32 [ARM_EXT_V6]    0xf8100a00 0xfe50ffff [arm_const "rfe", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], reg 16 19, arm_char1 21 21 '!']
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0070 0x0fff0ff0 (arm_cond $ SXT HalfWord <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0470 0x0fff0ff0 (arm_cond $ SXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0870 0x0fff0ff0 (arm_cond $ SXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06bf0c70 0x0fff0ff0 (arm_cond $ SXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x068f0070 0x0fff0ff0 (arm_cond $ SXTB16       <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x068f0470 0x0fff0ff0 (arm_cond $ SXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x068f0870 0x0fff0ff0 (arm_cond $ SXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x068f0c70 0x0fff0ff0 (arm_cond $ SXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06af0070 0x0fff0ff0 (arm_cond $ SXT Byte     <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06af0470 0x0fff0ff0 (arm_cond $ SXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06af0870 0x0fff0ff0 (arm_cond $ SXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06af0c70 0x0fff0ff0 (arm_cond $ SXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ff0070 0x0fff0ff0 (arm_cond $ UXT HalfWord <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ff0470 0x0fff0ff0 (arm_cond $ UXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ff0870 0x0fff0ff0 (arm_cond $ UXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ff0c70 0x0fff0ff0 (arm_cond $ UXT HalfWord <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06cf0070 0x0fff0ff0 (arm_cond $ UXTB16       <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06cf0470 0x0fff0ff0 (arm_cond $ UXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06cf0870 0x0fff0ff0 (arm_cond $ UXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06cf0c70 0x0fff0ff0 (arm_cond $ UXTB16       <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ef0070 0x0fff0ff0 (arm_cond $ UXT Byte     <$> reg 12 15 <*>               (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ef0470 0x0fff0ff0 (arm_cond $ UXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ef0870 0x0fff0ff0 (arm_cond $ UXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06ef0c70 0x0fff0ff0 (arm_cond $ UXT Byte     <$> reg 12 15 <*>               (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06b00070 0x0ff00ff0 (arm_cond $ SXTAH        <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06b00470 0x0ff00ff0 (arm_cond $ SXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06b00870 0x0ff00ff0 (arm_cond $ SXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06b00c70 0x0ff00ff0 (arm_cond $ SXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800070 0x0ff00ff0 (arm_cond $ SXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800470 0x0ff00ff0 (arm_cond $ SXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800870 0x0ff00ff0 (arm_cond $ SXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800c70 0x0ff00ff0 (arm_cond $ SXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00070 0x0ff00ff0 (arm_cond $ SXTAB        <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00470 0x0ff00ff0 (arm_cond $ SXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00870 0x0ff00ff0 (arm_cond $ SXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00c70 0x0ff00ff0 (arm_cond $ SXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06f00070 0x0ff00ff0 (arm_cond $ UXTAH        <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06f00470 0x0ff00ff0 (arm_cond $ UXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06f00870 0x0ff00ff0 (arm_cond $ UXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06f00c70 0x0ff00ff0 (arm_cond $ UXTAH        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06c00070 0x0ff00ff0 (arm_cond $ UXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06c00470 0x0ff00ff0 (arm_cond $ UXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06c00870 0x0ff00ff0 (arm_cond $ UXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06c00c70 0x0ff00ff0 (arm_cond $ UXTAB16      <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00070 0x0ff00ff0 (arm_cond $ UXTAB        <$> reg 12 15 <*> reg 16 19 <*> (Reg <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00470 0x0ff00ff0 (arm_cond $ UXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 8 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00870 0x0ff00ff0 (arm_cond $ UXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 16 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00c70 0x0ff00ff0 (arm_cond $ UXTAB        <$> reg 12 15 <*> reg 16 19 <*> (RegShiftImm S_ROR 24 <$> reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06800fb0 0x0ff00ff0 (arm_cond . reg12'15_reg16'19_reg0'3 $ SEL)
  , ARMOpcode32 [ARM_EXT_V6]    0xf1010000 0xfffffc00 (arm_uncond $ SETEND <$> (toEnum . fromIntegral . bit 9))
  , ARMOpcode32 [ARM_EXT_V6]    0x0700f010 0x0ff0f0d0 (arm_cond $ SMUAD  <$> toEnum . fromIntegral . bit 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11) -- TODO: double check enum direction is correct for first arg
  , ARMOpcode32 [ARM_EXT_V6]    0x0700f050 0x0ff0f0d0 (arm_cond $ SMUSD  <$> toEnum . fromIntegral . bit 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  , ARMOpcode32 [ARM_EXT_V6]    0x07000010 0x0ff000d0 (arm_cond $ SMLAD  <$> toEnum . fromIntegral . bit 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11 <*> reg 12 15)
  , ARMOpcode32 [ARM_EXT_V6]    0x07400010 0x0ff000d0 (arm_cond $ SMLALD <$> toEnum . fromIntegral . bit 5 <*> reg 12 15 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  , ARMOpcode32 [ARM_EXT_V6]    0x07000050 0x0ff000d0 (arm_cond $ SMLSD  <$> toEnum . fromIntegral . bit 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11 <*> reg 12 15) 
  , ARMOpcode32 [ARM_EXT_V6]    0x07400050 0x0ff000d0 (arm_cond $ SMLSLD <$> toEnum . fromIntegral . bit 5 <*> reg 12 15 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  , ARMOpcode32 [ARM_EXT_V6]    0x0750f010 0x0ff0f0d0 (arm_cond $ SMMUL  <$> bool 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11)
  , ARMOpcode32 [ARM_EXT_V6]    0x07500010 0x0ff000d0 (arm_cond $ SMMLA  <$> bool 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11 <*> reg 12 15)
  , ARMOpcode32 [ARM_EXT_V6]    0x075000d0 0x0ff000d0 (arm_cond $ SMMLS  <$> bool 5 <*> reg 16 19 <*> reg 0 3 <*> reg 8 11 <*> reg 12 15)
--, ARMOpcode32 [ARM_EXT_V6]    0xf84d0500 0xfe5fffe0 (arm_uncond $ SRS  <$> arm_arr 23 23 ['i' 'd'] arm_arr 24 24 ['b' 'a'] reg 16 19 arm_char1 21 21 '!' arm_d 0 4]
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00010 0x0fe00ff0 (arm_cond $ SSAT   <$> reg 12 15 <*> arm_W 16 20 <*> (Reg . reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00010 0x0fe00070 (arm_cond $ SSAT   <$> reg 12 15 <*> arm_W 16 20 <*> liftM2 (RegShiftImm S_LSL) (arm_d 7 11) (reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00050 0x0fe00070 (arm_cond $ SSAT   <$> reg 12 15 <*> arm_W 16 20 <*> liftM2 (RegShiftImm S_ASR) (arm_d 7 11) (reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06a00f30 0x0ff00ff0 (arm_cond $ SSAT16 <$> reg 12 15 <*> arm_W 16 19 <*> reg 0 3)
--, ARMOpcode32 [ARM_EXT_V6]    0x01800f90 0x0ff00ff0 (arm_cond $ STREX  <$> reg 12 15 <*> reg 12 15 <*> reg 0 3 <*> arm_square <*> reg 16 19) 
  , ARMOpcode32 [ARM_EXT_V6]    0x00400090 0x0ff000f0 (arm_cond $ UMAAL  <$> reg 12 15 <*> reg 12 15 <*> reg 16 19 <*> reg 0 3) -- (reg 8 11)) 
  , ARMOpcode32 [ARM_EXT_V6]    0x0780f010 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ USAD8 )
  , ARMOpcode32 [ARM_EXT_V6]    0x07800010 0x0ff000f0 (arm_cond . reg16'19_reg0'3_reg8'11_reg12'15 $ USADA8)
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00010 0x0fe00ff0 (arm_cond $ USAT   <$> reg 12 15 <*> arm_W 16 20 <*> Reg . reg 0 3)
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00010 0x0fe00070 (arm_cond $ USAT   <$> reg 12 15 <*> arm_W 16 20 <*> liftM2 (RegShiftImm S_LSL) (arm_d 7 11) (reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00050 0x0fe00070 (arm_cond $ USAT   <$> reg 12 15 <*> arm_W 16 20 <*> liftM2 (RegShiftImm S_ASR) (arm_d 7 11) (reg 0 3))
  , ARMOpcode32 [ARM_EXT_V6]    0x06e00f30 0x0ff00ff0 (arm_cond $ USAT16 <$> reg 12 15 <*> arm_W 16 19 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V5J]   0x012fff20 0x0ffffff0 (arm_cond $ BXJ <$> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V5]    0xe1200070 0xfff000f0 (arm_uncond $ BKPT <$> ((\x y -> x `shiftL` 4 .|. y)  <$> arm_d 8 19 <*> arm_d 0 3)) 
  , ARMOpcode32 [ARM_EXT_V5]    0xfa000000 0xfe000000 (arm_uncond $ BLXUC <$> arm_B)
  , ARMOpcode32 [ARM_EXT_V5]    0x012fff30 0x0ffffff0 (arm_cond $ BLX <$> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V5]    0x016f0f10 0x0fff0ff0 (arm_cond $ CLZ <$> reg 12 15 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V5E]   0x000000d0 0x0e1000f0 (arm_cond $ LDR DoubleWord False False <$> reg 12 15 <*> arm_s)
  , ARMOpcode32 [ARM_EXT_V5E]   0x000000f0 0x0e1000f0 (arm_cond $ STR DoubleWord False False <$> reg 12 15 <*> arm_s)
  , ARMOpcode32 [ARM_EXT_V5E]   0xf450f000 0xfc70f000 (arm_uncond $ PLD <$> arm_a)
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01000080 0x0ff000f0 (arm_cond . reg16'19_reg0'3_reg8'11_reg12'15 $ SMLA Low Low  )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000a0 0x0ff000f0 (arm_cond . reg16'19_reg0'3_reg8'11_reg12'15 $ SMLA High Low )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000c0 0x0ff000f0 (arm_cond . reg16'19_reg0'3_reg8'11_reg12'15 $ SMLA Low High )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000e0 0x0ff000f0 (arm_cond . reg16'19_reg0'3_reg8'11_reg12'15 $ SMLA High High)
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 (arm_cond . reg12'15_reg16'19_reg0'3_reg8'11 $ (SMLAL $ Just (Low, Low  )))
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 (arm_cond . reg12'15_reg16'19_reg0'3_reg8'11 $ (SMLAL $ Just (High, Low )))
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000c0 0x0ff000f0 (arm_cond . reg12'15_reg16'19_reg0'3_reg8'11 $ (SMLAL $ Just (Low, High )))
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000e0 0x0ff000f0 (arm_cond . reg12'15_reg16'19_reg0'3_reg8'11 $ (SMLAL $ Just (High, High)))
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01600080 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMUL Low Low  )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000a0 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMUL High Low )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000c0 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMUL Low High )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000e0 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMUL High High)
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x012000a0 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMULW Low )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x012000e0 0x0ff0f0f0 (arm_cond . reg16'19_reg0'3_reg8'11 $ SMULW High)
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01000050 0x0ff00ff0 (arm_cond . reg12'15_reg0'3_reg16'19 $ QADD )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01400050 0x0ff00ff0 (arm_cond . reg12'15_reg0'3_reg16'19 $ QDADD)
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01200050 0x0ff00ff0 (arm_cond . reg12'15_reg0'3_reg16'19 $ QSUB )
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01600050 0x0ff00ff0 (arm_cond . reg12'15_reg0'3_reg16'19 $ QDSUB)
  , ARMOpcode32 [ARM_EXT_V1]    0x00000090 0x0e100090 (arm_cond $ STR <$> arm_arr 5 5 [HalfWord, Byte] <*> pure False <*> bool 6 <*> reg 12 15 <*> arm_s)  --)[arm_const "str", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, reg 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V1]    0x00100090 0x0e100090 (arm_cond $ LDR <$> arm_arr 5 5 [HalfWord, Byte] <*> pure False <*> bool 6 <*> reg 12 15 <*> arm_s)  --)[arm_const "ldr", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, reg 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V1]    0x00000000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ AND)
  , ARMOpcode32 [ARM_EXT_V1]    0x00200000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ EOR)
  , ARMOpcode32 [ARM_EXT_V1]    0x00400000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ SUB)
  , ARMOpcode32 [ARM_EXT_V1]    0x00600000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ RSB)
  , ARMOpcode32 [ARM_EXT_V1]    0x00800000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ ADD)
  , ARMOpcode32 [ARM_EXT_V1]    0x00a00000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ ADC)
  , ARMOpcode32 [ARM_EXT_V1]    0x00c00000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ SBC)
  , ARMOpcode32 [ARM_EXT_V1]    0x00e00000 0x0de00000 (arm_cond . bool20_reg12'15_reg16'19_o $ RSC)
--, ARMOpcode32 [ARM_EXT_V3]    0x0120f000 0x0db0f000 (arm_cond $ MSR <$> arm_arr 22 22 [SPSR, CPSR] <*> arm_C <*> arm_o)
--, ARMOpcode32 [ARM_EXT_V3]    0x010f0000 0x0fbf0fff (arm_cond $ MRS <$> reg 12 15 <*> arm_arr 22 22 [SPSR, CPSR]))
  , ARMOpcode32 [ARM_EXT_V1]    0x01000000 0x0de00000 (arm_cond $ TST <$> {-arm_p <*>-} reg 16 19 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x01200000 0x0de00000 (arm_cond $ TEQ <$> {-arm_p <*>-} reg 16 19 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x01400000 0x0de00000 (arm_cond $ CMP <$> {-arm_p <*>-} reg 16 19 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x01600000 0x0de00000 (arm_cond $ CMN <$> {-arm_p <*>-} reg 16 19 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x01800000 0x0de00000 (arm_cond $ ORR <$> bool 20 <*> reg 12 15 <*> reg 16 19 <*> arm_o)
                                
  , ARMOpcode32 [ARM_EXT_V1]    0x03a00000 0x0fef0000 (arm_cond $ MOV <$> bool 20 <*> reg 12 15 <*> arm_o)
--, ARMOpcode32 [ARM_EXT_V1]    0x01a00000 0x0def0ff0 (arm_cond $ MOV <$> bool 20 <*> reg 12 15 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V1]    0x01a00000 0x0def0060 (arm_cond $ LSL <$> bool 20 <*> reg 12 15 <*> arm_q)
  , ARMOpcode32 [ARM_EXT_V1]    0x01a00020 0x0def0060 (arm_cond $ LSR <$> bool 20 <*> reg 12 15 <*> arm_q)
  , ARMOpcode32 [ARM_EXT_V1]    0x01a00040 0x0def0060 (arm_cond $ ASR <$> bool 20 <*> reg 12 15 <*> arm_q)
  , ARMOpcode32 [ARM_EXT_V1]    0x01a00060 0x0def0ff0 (arm_cond $ RRX <$> bool 20 <*> reg 12 15 <*> reg 0 3)
  , ARMOpcode32 [ARM_EXT_V1]    0x01a00060 0x0def0060 (arm_cond $ ROR <$> bool 20 <*> reg 12 15 <*> arm_q)
                                
  , ARMOpcode32 [ARM_EXT_V1]    0x01c00000 0x0de00000 (arm_cond $ BIC <$> bool 20 <*> reg 12 15 <*> reg 16 19 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x01e00000 0x0de00000 (arm_cond $ MVN <$> bool 20 <*> reg 12 15 <*> arm_o)
  , ARMOpcode32 [ARM_EXT_V1]    0x052d0004 0x0fff0fff (arm_cond $ STR Word False False <$> reg 12 15 <*> arm_a)
  , ARMOpcode32 [ARM_EXT_V1]    0x04000000 0x0e100000 (arm_cond $ STR <$> arm_bw 22 <*> arm_t <*> pure False <*> reg 12 15 <*> arm_a)
  , ARMOpcode32 [ARM_EXT_V1]    0x06000000 0x0e100ff0 (arm_cond $ STR <$> arm_bw 22 <*> arm_t <*> pure False <*> reg 12 15 <*> arm_a)
  , ARMOpcode32 [ARM_EXT_V1]    0x04000000 0x0c100010 (arm_cond $ STR <$> arm_bw 22 <*> arm_t <*> pure False <*> reg 12 15 <*> arm_a)
--, ARMOpcode32 [ARM_EXT_V1]    0x06000010 0x0e000010 [arm_const "undefined"]
  , ARMOpcode32 [ARM_EXT_V1]    0x049d0004 0x0fff0fff (arm_cond $ LDR Word False False <$> reg 12 15 <*> arm_a)
  , ARMOpcode32 [ARM_EXT_V1]    0x04100000 0x0c100000 (arm_cond $ LDR <$> arm_bw 22 <*> arm_t <*> pure False <*> reg 12 15 <*> arm_a)
  , ARMOpcode32 [ARM_EXT_V1]    0x092d0000 0x0fff0000 (arm_cond $ PUSH <$> arm_m)
--, ARMOpcode32 [ARM_EXT_V1]    0x08800000 0x0ff00000 [arm_const "stm", arm_c, reg 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
--, ARMOpcode32 [ARM_EXT_V1]    0x08000000 0x0e100000 [arm_const "stm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, reg 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1]    0x08bd0000 0x0fff0000 (arm_cond $ POP <$> arm_m)
--, ARMOpcode32 [ARM_EXT_V1]    0x08900000 0x0f900000 [arm_const "ldm", arm_c, reg 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
--, ARMOpcode32 [ARM_EXT_V1]    0x08100000 0x0e100000 [arm_const "ldm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, reg 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1]    0x0a000000 0x0e000000 (arm_cond $ B <$> bool 24 <*> arm_b)
--, ARMOpcode32 [ARM_EXT_V1]    0x0f000000 0x0f000000 [arm_const "svc", arm_c, arm_x 0 23] -- does this belong?
--, ARMOpcode32 [ARM_EXT_V1]    0x00000000 0x00000000 [arm_const "undefined instruction", arm_x 0 31]  
  ]

armOpcodeMatches :: Word32 -> ARMOpcode32 -> Bool
armOpcodeMatches x (ARMOpcode32 _ v m _) = x .&. m == v 

armDecodeOp :: Word32 -> ARMOpcode32 -> ARMInstruction
armDecodeOp x (ARMOpcode32 _ _ _ d) = d x

armDecode :: Word32 -> Maybe ARMInstruction
armDecode i = fmap (armDecodeOp i) . find (armOpcodeMatches i) $ armOpcodes