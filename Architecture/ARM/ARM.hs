module Architecture.ARM.ARM where

import Architecture.ARM.Common

import Data.Maybe
import Data.List
import Data.Int
import Data.Word
import Data.Bits

import Text.Printf

import Control.Monad
import Control.Applicative

{-
data ARMOpRegister = OP_Reg ARMRegister
                   | OP_RegBang ARMRegister
  deriving (Show, Read, Eq)
-}

data ARMOpData = OP_Imm Int
               | OP_Reg ARMRegister
               | OP_RegShiftImm ARMShift Int ARMRegister 
               | OP_RegShiftReg ARMShift ARMRegister ARMRegister
               | OP_RegShiftRRX ARMRegister
  deriving (Show, Read, Eq)
  
data ARMOpMemory = OP_MemReg ARMRegister ARMOpData Bool
                 | OP_MemRegNeg ARMRegister ARMOpData Bool
                 | OP_MemRegPost ARMRegister ARMOpData
                 | OP_MemRegPostNeg ARMRegister ARMOpData
  deriving (Show, Read, Eq)

data ARMOpMultiple = OP_Regs [ARMRegister]
                   | OP_RegsCaret [ARMRegister]
  deriving (Show, Read, Eq)

data ARMInstruction = ARMUnconditionalInstruction ARMUnconditionalOpcode
                    | ARMConditionalInstruction ARMCondition ARMConditionalOpcode
  deriving (Show, Read, Eq)

data ARMConditionalOpcode = O_B Bool Int32 -- B, BL
                          | O_BLX ARMRegister -- conditional form
                          | O_BX ARMRegister
                          | O_BXJ ARMRegister
                          
                          | O_AND Bool ARMRegister ARMRegister ARMOpData -- AND, ANDS
                          | O_EOR Bool ARMRegister ARMRegister ARMOpData -- EOR, EORS
                          | O_SUB Bool ARMRegister ARMRegister ARMOpData -- SUB, SUBS
                          | O_RSB Bool ARMRegister ARMRegister ARMOpData -- RSB, RSBS
                          | O_ADD Bool ARMRegister ARMRegister ARMOpData -- ADD, ADDS
                          | O_ADC Bool ARMRegister ARMRegister ARMOpData -- ADC, ADCS
                          | O_SBC Bool ARMRegister ARMRegister ARMOpData -- SBC, SBCS
                          | O_RSC Bool ARMRegister ARMRegister ARMOpData -- RSC, RSCS
                          | O_TST ARMRegister ARMOpData -- TST
                          | O_TEQ ARMRegister ARMOpData -- TEQ
                          | O_CMP ARMRegister ARMOpData -- CMP
                          | O_CMN ARMRegister ARMOpData -- CMN
                          | O_ORR Bool ARMRegister ARMRegister ARMOpData -- ORR, ORRS
                          | O_MOV Bool ARMRegister ARMOpData -- MOV, MOVS
                          | O_LSL Bool ARMRegister ARMOpData
                          | O_LSR Bool ARMRegister ARMOpData
                          | O_ASR Bool ARMRegister ARMOpData
                          | O_RRX Bool ARMRegister ARMRegister 
                          | O_ROR Bool ARMRegister ARMOpData
                          | O_BIC Bool ARMRegister ARMRegister ARMOpData -- BIC, BICS
                          | O_MVN Bool ARMRegister ARMOpData -- MVN, MVNS
                                                    
                          | O_MLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_MUL Bool ARMRegister ARMRegister ARMRegister 
                          | O_SMLA Nybble Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- SMLABB, SMLABT, SBMLATB, SMLATT
                          | O_SMLAD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMLAL (Maybe (Nybble, Nybble)) ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME: first two ArmRegisters are more complicated
                          | O_SMLALD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
                          | O_SMLAW Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMLSD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMLSLD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
                          | O_SMMLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMMUL Bool ARMRegister ARMRegister ARMRegister 
                          | O_SMMLS Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMUAD Nybble ARMRegister ARMRegister ARMRegister
                          | O_SMUL Nybble Nybble ARMRegister ARMRegister ARMRegister
                          | O_SMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_SMULW Nybble ARMRegister ARMRegister ARMRegister
                          | O_SMUSD Nybble ARMRegister ARMRegister ARMRegister
                          | O_UMAAL ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_UMLAL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_UMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          
                          | O_QADD ARMRegister ARMRegister ARMRegister
                          | O_QADD16 ARMRegister ARMRegister ARMRegister
                          | O_QADD8 ARMRegister ARMRegister ARMRegister
                          | O_QADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_QDADD ARMRegister ARMRegister ARMRegister
                          | O_QDSUB ARMRegister ARMRegister ARMRegister
                          | O_QSUB ARMRegister ARMRegister ARMRegister
                          | O_QSUB16 ARMRegister ARMRegister ARMRegister
                          | O_QSUB8 ARMRegister ARMRegister ARMRegister
                          | O_QSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_SADD16 ARMRegister ARMRegister ARMRegister
                          | O_SADD8 ARMRegister ARMRegister ARMRegister
                          | O_SADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_SSUB16 ARMRegister ARMRegister ARMRegister
                          | O_SSUB8 ARMRegister ARMRegister ARMRegister
                          | O_SSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_SHADD16 ARMRegister ARMRegister ARMRegister
                          | O_SHADD8 ARMRegister ARMRegister ARMRegister
                          | O_SHADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_SHSUB16 ARMRegister ARMRegister ARMRegister
                          | O_SHSUB8 ARMRegister ARMRegister ARMRegister
                          | O_SHSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_UADD16 ARMRegister ARMRegister ARMRegister
                          | O_UADD8 ARMRegister ARMRegister ARMRegister
                          | O_UADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_USUB16 ARMRegister ARMRegister ARMRegister
                          | O_USUB8 ARMRegister ARMRegister ARMRegister
                          | O_USUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_UHADD16 ARMRegister ARMRegister ARMRegister
                          | O_UHADD8 ARMRegister ARMRegister ARMRegister
                          | O_UHADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_UHSUB16 ARMRegister ARMRegister ARMRegister
                          | O_UHSUB8 ARMRegister ARMRegister ARMRegister
                          | O_UHSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_UQADD16 ARMRegister ARMRegister ARMRegister
                          | O_UQADD8 ARMRegister ARMRegister ARMRegister
                          | O_UQADDSUBX ARMRegister ARMRegister ARMRegister
                          | O_UQSUB16 ARMRegister ARMRegister ARMRegister
                          | O_UQSUB8 ARMRegister ARMRegister ARMRegister
                          | O_UQSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | O_SXTAB16 ARMRegister ARMRegister ARMOpData -- rotate
                          | O_SXTAB ARMRegister ARMRegister ARMOpData -- rotate
                          | O_SXTAH ARMRegister ARMRegister ARMOpData -- rotate
                          | O_SXTB16 ARMRegister ARMOpData -- rotate
                          | O_SXT Width ARMRegister ARMOpData --rotate only -- ONLY SXTB, SXTH
                          | O_UXTAB16 ARMRegister ARMRegister ARMOpData -- rotate 
                          | O_UXTAB ARMRegister ARMRegister ARMOpData -- rotate
                          | O_UXTAH ARMRegister ARMRegister ARMOpData -- rotate
                          | O_UXTB16 ARMRegister ARMOpData -- rotate
                          | O_UXT Width ARMRegister ARMOpData -- rotate -- UXTB, UXTH
                          
                          | O_CLZ ARMRegister ARMRegister
                          | O_USAD8 ARMRegister ARMRegister ARMRegister 
                          | O_USADA8 ARMRegister ARMRegister ARMRegister ARMRegister
                          | O_PKHBT ARMRegister ARMRegister ARMOpData -- rotate/shift
                          | O_PKHTB ARMRegister ARMRegister ARMOpData -- rotate/shift
                          | O_REV ARMRegister ARMRegister
                          | O_REV16 ARMRegister ARMRegister
                          | O_REVSH ARMRegister ARMRegister
                          | O_SEL ARMRegister ARMRegister ARMRegister
                          | O_SSAT ARMRegister Word8 ARMOpData -- rotate/shift
                          | O_SSAT16 ARMRegister Word8 ARMRegister
                          | O_USAT ARMRegister Word8 ARMOpData -- rotate/shift
                          | O_USAT16 ARMRegister Word8 ARMRegister
                          
                          | O_MRS ARMRegister ARMStatusRegister
                          | O_MSR 
                          
                          | O_LDR Width Bool Bool ARMRegister ARMOpMemory -- LDR, LDRB, LDRH, LDRD, LDRT LDRBT, LDRSB, LDRSH -- TODO: some of these combinations are invalid, should we stop that?
                          | O_STR Width Bool Bool ARMRegister ARMOpMemory -- STR, STRB, STRH, STRD, STRT, STRBT
                          | O_LDREX
                          | O_STREX
                          
                          | O_LDM -- Note that there are three different forms
                          | O_STM -- Note that there are two different forms
                          | O_PUSH [ARMRegister]
                          | O_POP [ARMRegister]
                          
                          | O_SWP Bool ARMRegister ARMRegister ARMRegister -- SWP, SWPB
                          
                          | O_SWI Word32
                          
                          | O_DBG Word32
                          | O_DMB ARMHint
                          | O_DSB ARMHint
                          | O_ISB ARMHint
                          
                          | O_PLI ARMOpMemory
                          
                          | O_YIELD
                          | O_WFE
                          | O_WFI
                          | O_SEV
                          
                          | O_BFC ARMRegister (Maybe (Word32, Word32))
                          | O_BFI ARMRegister ARMRegister (Maybe (Word32, Word32)) -- come up with a nicer way to do this
                          | O_MLS ARMRegister ARMRegister ARMRegister
                          
                          | O_MOVW ARMRegister Word32
                          | O_MOVT ARMRegister Word32
                          | O_RBIT ARMRegister ARMRegister
  deriving (Show, Read, Eq)

data ARMUnconditionalOpcode = O_CPS
                            | O_SETEND ARMEndian
                            | O_RFE
                            | O_BKPT Word8
                            | O_PLD ARMOpMemory
                            | O_SRS
                            | O_BLXUC Int32 -- unconditional BLX
  deriving (Show, Read, Eq)

bitRange :: (Integral a, Bits a) => Int -> Int -> a -> a
bitRange start end i = ((i `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1))

type ARMDecoder a = (ARMState, Word32) -> a

armDecodeAddress :: ARMDecoder ARMOpMemory
armDecodeAddress (s, a) | (a .&. 0xf0000) == 0xf0000 && (a .&. 0x2000000) == 0 = 
                              let offset = a .&. 0xfff in
                                case a .&. 0x1000000 /= 0 of
                                  True -> OP_MemReg PC (OP_Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)) ((a .&. 0x200000) /= 0)
                                  _    -> OP_MemRegPost PC $ OP_Imm (fromIntegral offset)
                        | otherwise = 
                              let baseReg = (toEnum (((fromIntegral a) `shiftR` 16 ) .&. 0xf)) in case a .&. 0x1000000 /= 0 of
                                False -> if (a .&. 0x2000000) == 0 then
                                           let offset = a .&. 0xfff in
                                             if offset /= 0 then
                                               OP_MemRegPost baseReg $ OP_Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)
                                               else OP_MemRegPost baseReg $ OP_Imm 0
                                           else (if (a .&. 0x800000) == 0 then OP_MemRegPostNeg else OP_MemRegPost) baseReg (armDecodeShift a False)
                                _     -> if (a .&. 0x2000000) == 0 then
                                           let offset = a .&. 0xfff in
                                             OP_MemReg baseReg (OP_Imm (if (a .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset)) ((a .&. 0x200000) /= 0)
                                           else (if (a .&. 0x800000) == 0 then OP_MemRegNeg else OP_MemReg) baseReg (armDecodeShift a False) ((a .&. 0x200000) /= 0)

armDecodeShift :: Word32 -> Bool -> ARMOpData
armDecodeShift i p =  if i .&. 0xff0 /= 0 then
                        if i .&. 0x10 == 0 then
                          let amount = (i .&. 0xf80) `shiftR` 7
                              shift = ((fromIntegral i) .&. 0x60) `shiftR` 5 in
                            if amount == 0 && shift == 3 then  OP_RegShiftRRX (toEnum ((fromIntegral i) .&. 0xf)) 
                              else  OP_RegShiftImm (toEnum shift) (fromIntegral amount) (toEnum ((fromIntegral i) .&. 0xf)) 
                          else  OP_RegShiftImm (toEnum (((fromIntegral i) .&. 0x60) `shiftR` 5)) (toEnum (((fromIntegral i) .&. 0xf00 `shiftR` 8))) (toEnum ((fromIntegral i) .&. 0xf)) 
                        else OP_Reg (toEnum ((fromIntegral i) .&. 0xf))

arm_const :: String -> ARMDecoder String
arm_const x (s, i) = x

arm_constint :: Int -> ARMDecoder String
arm_constint x (s, i) = show x

arm_a :: ARMDecoder ARMOpMemory
arm_a = armDecodeAddress 

-- FIXME: wow, this is pretty ugly...
arm_s :: ARMDecoder ARMOpMemory
arm_s (s, i) | i .&. 0x4f0000 == 0x4f0000 = OP_MemReg PC (OP_Imm (fromIntegral $ (if i .&. 0x800000 == 0 then -1 else 1) * ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf))) False
             | i .&. 0x1000000 /= 0 = case i .&. 0x400000 of
                 0x400000 -> OP_MemReg (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) 
                                 (OP_Imm $ let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                                   if (i .&. 0x800000) == 0 then -(fromIntegral offset) else fromIntegral offset) 
                                    ((i .&. 0x200000) /= 0)
                 _        -> (if (i .&. 0x800000) == 0 then OP_MemRegNeg else OP_MemReg) (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf))
                                 (OP_Reg $ toEnum ((fromIntegral i) .&. 0xf)) ((i .&. 0x200000) /= 0)
             | otherwise = case i .&. 0x400000 of
                 0x400000 -> OP_MemReg (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) 
                                 (OP_Imm $ let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                                   (if (i .&. 0x800000) == 0 then  -(fromIntegral offset) else fromIntegral offset))
                                    False
                 _        -> (if (i .&. 0x800000) == 0 then OP_MemRegPostNeg else OP_MemRegPost) (toEnum (((fromIntegral i) `shiftR` 16) .&. 0xf)) (OP_Reg $ toEnum ((fromIntegral i) .&. 0xf))

arm_b :: ARMDecoder Int32
arm_b (s, i) = ((((fromIntegral i :: Int32) .&. 0xffffff) `xor` 0x800000) - 0x800000) * 4 + (fromIntegral $ pc s) + 8

arm_c :: ARMDecoder ARMCondition
arm_c (_, i) = toEnum $ fromIntegral ((i `shiftR` 28) .&. 0xf)

arm_m :: ARMDecoder ARMOpMultiple
arm_m (s, i) = OP_Regs . catMaybes $ map (\x -> if i .&. (1 `shiftL` x) /= 0 then Just $ toEnum x else Nothing) [0..15]

arm_o :: ARMDecoder ARMOpData
arm_o (_, i) | i .&. 0x2000000 /= 0 = OP_Imm . fromIntegral $ (i .&. 0xff) `rotateR` (((fromIntegral i) .&. 0xf00) `shiftR` 7)
             | otherwise = armDecodeShift i True

arm_p :: ARMDecoder Bool
arm_p (s, i) = i .&. 0xf000 == 0xf000

arm_t :: ARMDecoder Bool
arm_t (s, i) = i .&. 0x1200000 == 0x200000

arm_q :: ARMDecoder ARMOpData
arm_q (s, i) = armDecodeShift i False

arm_e :: ARMDecoder Word32
arm_e (s, i) = (i .&. 0xf) .|. ((i .&. 0xfff00) `shiftR` 4)

arm_B :: ARMDecoder Int32
arm_B (s, i) = let offset = ((if i .&. 0x800000 /= 0 then 0xff else 0) + (i .&. 0xffffff)) `shiftL` 2 
                   address = offset + (pc s) + 8 + (if i .&. 0x1000000 /= 0 then 2 else 0) in
                     fromIntegral address
              
-- FIXME: this is ugly
arm_C :: ARMDecoder String
arm_C (s, i) = '_' : (if i .&. 0x80000 /= 0 then "f" else "" ++ 
                   if i .&. 0x40000 /= 0 then "s" else "" ++
                   if i .&. 0x20000 /= 0 then "x" else "" ++
                   if i .&. 0x10000 /= 0 then "c" else "")

arm_U :: ARMDecoder ARMHint
arm_U (s, i) = case i .&. 0xf of
                 0xf -> SY
                 0x7 -> UN
                 0xe -> ST
                 0x6 -> UNST
                 x   -> UK x

arm_P :: ARMDecoder ARMOpMemory
arm_P (s, i) = armDecodeAddress (s, (i .|. (1 `shiftL` 24)))

arm_r :: Int -> Int -> ARMDecoder ARMRegister
arm_r start end (_, i) = toEnum (bitRange start end $ fromIntegral i)

arm_d :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_d start end (_, i) = bitRange start end $ fromIntegral i

arm_W :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_W start end (_, i) = (+1) . bitRange start end $ fromIntegral i

arm_x :: (Integral a, Bits a) => Int -> Int -> ARMDecoder a
arm_x = arm_d

arm_X :: Int -> Int -> ARMDecoder Word32
arm_X start end (s, i) = (.&. 0xf) . bitRange start end $ i

arm_arr :: Int -> Int -> [a] -> ARMDecoder a
arm_arr start end c (_, i) = c !! (fromIntegral $ bitRange start end i)

arm_E :: ARMDecoder (Maybe (Word32, Word32))
arm_E (_, i) = let msb = (i .&. 0x1f0000) `shiftR` 16
                   lsb = (i .&. 0xf80) `shiftR` 7
                   width = msb - lsb + 1 in
                 if width > 0 then
                   Just (lsb, width) --"#" ++ (show lsb) ++ ", #" ++ (show width)
                   else Nothing --"(invalid " ++ (show lsb) ++ ":" ++ (show msb) ++ ")"            

arm_V :: ARMDecoder Word32
arm_V (_, i) = (i .&. 0xf0000) `shiftR` 4 .|. (i .&. 0xfff)

{-
arm_square :: ARMDecoder -> ARMDecoder
arm_square d = ((("[" ++) . (++ "]")) .) . d

arm_curly :: ARMDecoder -> ARMDecoder
arm_curly d = ((("{" ++) . (++ "}")) .) . d

-}

arm_bit bit (_, i) = bitRange bit bit i

arm_bool bit s = arm_bit bit s == 1

arm_uncond = liftM ARMUnconditionalInstruction

arm_cond = liftM2 ARMConditionalInstruction arm_c

arm_bw bit (_, i) = if bitRange bit bit i == 1 then Byte else Word

armOpcodes = 
  [--ARMOpcode32 [ARM_EXT_V1] 0xe1a00000 0xffffffff [arm_const "nop"]
   ARMOpcode32 [ARM_EXT_V4T, ARM_EXT_V5] 0x012FFF10 0x0ffffff0 (arm_cond $ liftM O_BX (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V2] 0x00000090 0x0fe000f0 (arm_cond $ liftM4 O_MUL (arm_bool 20) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V2] 0x00200090 0x0fe000f0 (arm_cond $ liftM5 O_MLA (arm_bool 20) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  --, ARMOpcode32 [ARM_EXT_V2S] 0x01000090 0x0fb00ff0 [arm_const "swp", arm_char1 22 22 'b', arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)]
  ,ARMOpcode32 [ARM_EXT_V3M] 0x00800090 0x0fa000f0 (arm_cond $ (arm_arr 22 22 [O_SMULL, O_UMULL]) `ap` (arm_bool 20) `ap` (arm_r 12 15) `ap` (arm_r 16 19) `ap` (arm_r 0 3) `ap` (arm_r 8 11))
  --,ARMOpcode32 [ARM_EXT_V3M] 0x00800090 0x0fa000f0 (arm_cond $ (arm_arr 22 22 [O_SMLAL, O_UMLAL]) `ap` (arm_bool 20) `ap` (arm_r 12 15) `ap` (arm_r 16 19) `ap` (arm_r 0 3) `ap` (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V7] 0xf450f000 0xfd70f000 (arm_cond $ liftM O_PLI arm_P)
  ,ARMOpcode32 [ARM_EXT_V7] 0x0320f0f0 0x0ffffff0 (arm_cond $ liftM O_DBG (arm_d 0 3))
  ,ARMOpcode32 [ARM_EXT_V7] 0xf57ff050 0x0ffffff0 (arm_cond $ liftM O_DMB arm_U)
  ,ARMOpcode32 [ARM_EXT_V7] 0xf57ff040 0x0ffffff0 (arm_cond $ liftM O_DSB arm_U)
  ,ARMOpcode32 [ARM_EXT_V7] 0xf57ff060 0x0ffffff0 (arm_cond $ liftM O_ISB arm_U)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x07c0001f 0x0fe0007f (arm_cond $ liftM2 O_BFC (arm_r 12 15) arm_E)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x07c00010 0x0fe00070 (arm_cond $ liftM3 O_BFI (arm_r 12 15) (arm_r 0 3) arm_E)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x00600090 0x0ff000f0 (arm_cond $ liftM3 O_MLS (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x006000b0 0x0f7000f0 (arm_cond $ liftM2 (O_STR HalfWord True False) (arm_r 12 15) arm_s) -- TODO: check me
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x00300090 0x0f300090 (arm_cond $ liftM5 O_LDR (arm_arr 5 5 [HalfWord, Byte]) (const False) (arm_bool 6) (arm_r 12 15) arm_s)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x03000000 0x0ff00000 (arm_cond $ liftM2 O_MOVW (arm_r 12 15) arm_V)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x03400000 0x0ff00000 (arm_cond $ liftM2 O_MOVT (arm_r 12 15) arm_V)
  ,ARMOpcode32 [ARM_EXT_V6T2] 0x06ff0f30 0x0fff0ff0 (arm_cond $ liftM2 O_RBIT (arm_r 12 15) (arm_r 0 3))
  --, ARMOpcode32 [ARM_EXT_V6T2] 0x07a00050 0x0fa00070 [arm_arr 22 22 ['u', 's'], arm_const "bfx", arm_c, arm_r 12 15, arm_r 0 3, arm_d 7 11, arm_W 16 20]
  --, ARMOpcode32 [ARM_EXT_V6Z] 0x01600070 0x0ff000f0 [arm_const "smc", arm_c, arm_e]
  --, ARMOpcode32 [ARM_EXT_V6K] 0xf57ff01f 0xffffffff [arm_const "clrex"]
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01d00f9f 0x0ff00fff [arm_const "ldrexb", arm_c, arm_r 12 15, arm_square (arm_r 16 19)]
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01b00f9f 0x0ff00fff [arm_const "ldrexd", arm_c, arm_r 12 15, arm_square (arm_r 16 19)] 
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01f00f9f 0x0ff00fff [arm_const "ldrexh", arm_c, arm_r 12 15, arm_square (arm_r 16 19)] 
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01c00f90 0x0ff00ff0 [arm_const "strexb", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)]
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01a00f90 0x0ff00ff0 [arm_const "strexd", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)] 
  --, ARMOpcode32 [ARM_EXT_V6K] 0x01e00f90 0x0ff00ff0 [arm_const "strexh", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)] 
  ,ARMOpcode32 [ARM_EXT_V6K] 0x0320f001 0x0fffffff (arm_cond $ const O_YIELD)
  ,ARMOpcode32 [ARM_EXT_V6K] 0x0320f002 0x0fffffff (arm_cond $ const O_WFE)
  ,ARMOpcode32 [ARM_EXT_V6K] 0x0320f003 0x0fffffff (arm_cond $ const O_WFI)
  ,ARMOpcode32 [ARM_EXT_V6K] 0x0320f004 0x0fffffff (arm_cond $ const O_SEV)
  --, ARMOpcode32 [ARM_EXT_V6K] 0x0320f000 0x0fffff00 [arm_const "nop", arm_c, arm_curly (arm_d 0 7)]
  --, ARMOpcode32 [ARM_EXT_V6] 0xf1080000 0xfffffe3f [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f']
  --, ARMOpcode32 [ARM_EXT_V6] 0xf10a0000 0xfffffe20 [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
  --, ARMOpcode32 [ARM_EXT_V6] 0xf10C0000 0xfffffe3f [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f'] 
  --, ARMOpcode32 [ARM_EXT_V6] 0xf10e0000 0xfffffe20 [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
  --, ARMOpcode32 [ARM_EXT_V6] 0xf1000000 0xfff1fe20 [arm_const "cps", arm_d 0 4]
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800010 0x0ff00ff0 (arm_cond $ liftM3 O_PKHBT     (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800010 0x0ff00070 (arm_cond $ liftM3 O_PKHBT     (arm_r 12 15) (arm_r 16 19) (liftM2 (OP_RegShiftImm S_LSL) (arm_d 7 11) (arm_r 0 3)))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800050 0x0ff00ff0 (arm_cond $ liftM3 O_PKHTB     (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ASR 32 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800050 0x0ff00070 (arm_cond $ liftM3 O_PKHTB     (arm_r 12 15) (arm_r 16 19) (liftM2 (OP_RegShiftImm S_ASR) (arm_d 7 11) (arm_r 0 3)))
  --,ARMOpcode32 [ARM_EXT_V6] 0x01900f9f 0x0ff00fff (arm_cond $ liftM3 O_LDREX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200f10 0x0ff00ff0 (arm_cond $ liftM3 O_QADD16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200f90 0x0ff00ff0 (arm_cond $ liftM3 O_QADD8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200f30 0x0ff00ff0 (arm_cond $ liftM3 O_QADDSUBX  (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200f70 0x0ff00ff0 (arm_cond $ liftM3 O_QSUB16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200ff0 0x0ff00ff0 (arm_cond $ liftM3 O_QSUB8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06200f50 0x0ff00ff0 (arm_cond $ liftM3 O_QSUBADDX  (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06100f10 0x0ff00ff0 (arm_cond $ liftM3 O_SADD16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06100f90 0x0ff00ff0 (arm_cond $ liftM3 O_SADD8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  --,ARMOpcode32 [ARM_EXT_V6] 0x06100f30 0x0ff00ff0 (arm_cond $ liftM3 O_SADDADDX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3)) -- http://sourceware.org/bugzilla/show_bug.cgi?id=6773
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300f10 0x0ff00ff0 (arm_cond $ liftM3 O_SHADD16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300f90 0x0ff00ff0 (arm_cond $ liftM3 O_SHADD8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300f30 0x0ff00ff0 (arm_cond $ liftM3 O_SHADDSUBX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300f70 0x0ff00ff0 (arm_cond $ liftM3 O_SHSUB16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300ff0 0x0ff00ff0 (arm_cond $ liftM3 O_SHSUB8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06300f50 0x0ff00ff0 (arm_cond $ liftM3 O_SHSUBADDX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06100f70 0x0ff00ff0 (arm_cond $ liftM3 O_SSUB16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06100ff0 0x0ff00ff0 (arm_cond $ liftM3 O_SSUB8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06100f50 0x0ff00ff0 (arm_cond $ liftM3 O_SSUBADDX  (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500f10 0x0ff00ff0 (arm_cond $ liftM3 O_UADD16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500f90 0x0ff00ff0 (arm_cond $ liftM3 O_UADD8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500f30 0x0ff00ff0 (arm_cond $ liftM3 O_UADDSUBX  (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700f10 0x0ff00ff0 (arm_cond $ liftM3 O_UHADD16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700f90 0x0ff00ff0 (arm_cond $ liftM3 O_UHADD8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700f30 0x0ff00ff0 (arm_cond $ liftM3 O_UHADDSUBX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700f70 0x0ff00ff0 (arm_cond $ liftM3 O_UHSUB16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700ff0 0x0ff00ff0 (arm_cond $ liftM3 O_UHSUB8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06700f50 0x0ff00ff0 (arm_cond $ liftM3 O_UHSUBADDX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600f10 0x0ff00ff0 (arm_cond $ liftM3 O_UQADD16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600f90 0x0ff00ff0 (arm_cond $ liftM3 O_UQADD8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600f30 0x0ff00ff0 (arm_cond $ liftM3 O_UQADDSUBX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600f70 0x0ff00ff0 (arm_cond $ liftM3 O_UQSUB16   (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600ff0 0x0ff00ff0 (arm_cond $ liftM3 O_UQSUB8    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06600f50 0x0ff00ff0 (arm_cond $ liftM3 O_UQSUBADDX (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500f70 0x0ff00ff0 (arm_cond $ liftM3 O_USUB16    (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500ff0 0x0ff00ff0 (arm_cond $ liftM3 O_USUB8     (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06500f50 0x0ff00ff0 (arm_cond $ liftM3 O_USUBADDX  (arm_r 12 15) (arm_r 16 19) (arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0f30 0x0fff0ff0 (arm_cond $ liftM2 O_REV       (arm_r 12 15) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0fb0 0x0fff0ff0 (arm_cond $ liftM2 O_REV16     (arm_r 12 15) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ff0fb0 0x0fff0ff0 (arm_cond $ liftM2 O_REVSH     (arm_r 12 15) (arm_r 0 3))
  --, ARMOpcode32 [ARM_EXT_V6] 0xf8100a00 0xfe50ffff [arm_const "rfe", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_r 16 19, arm_char1 21 21 '!']
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0070 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT HalfWord) (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0470 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0870 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06bf0c70 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x068f0070 0x0fff0ff0 (arm_cond $ liftM2 O_SXTB16         (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x068f0470 0x0fff0ff0 (arm_cond $ liftM2 O_SXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x068f0870 0x0fff0ff0 (arm_cond $ liftM2 O_SXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x068f0c70 0x0fff0ff0 (arm_cond $ liftM2 O_SXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06af0070 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT Byte)     (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06af0470 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06af0870 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06af0c70 0x0fff0ff0 (arm_cond $ liftM2 (O_SXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ff0070 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT HalfWord) (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ff0470 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ff0870 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ff0c70 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT HalfWord) (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06cf0070 0x0fff0ff0 (arm_cond $ liftM2 O_UXTB16         (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06cf0470 0x0fff0ff0 (arm_cond $ liftM2 O_UXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06cf0870 0x0fff0ff0 (arm_cond $ liftM2 O_UXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06cf0c70 0x0fff0ff0 (arm_cond $ liftM2 O_UXTB16         (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ef0070 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT Byte)     (arm_r 12 15) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ef0470 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ef0870 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06ef0c70 0x0fff0ff0 (arm_cond $ liftM2 (O_UXT Byte)     (arm_r 12 15) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06b00070 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAH          (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06b00470 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06b00870 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06b00c70 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800070 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800470 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))  
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800870 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800c70 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00070 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB          (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00470 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00870 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00c70 0x0ff00ff0 (arm_cond $ liftM3 O_SXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06f00070 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAH          (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06f00470 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))  
  ,ARMOpcode32 [ARM_EXT_V6] 0x06f00870 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06f00c70 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAH          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06c00070 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06c00470 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))  
  ,ARMOpcode32 [ARM_EXT_V6] 0x06c00870 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06c00c70 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB16        (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00070 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB          (arm_r 12 15) (arm_r 16 19) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00470 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 8 . arm_r 0 3))  
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00870 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 16 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00c70 0x0ff00ff0 (arm_cond $ liftM3 O_UXTAB          (arm_r 12 15) (arm_r 16 19) (OP_RegShiftImm S_ROR 24 . arm_r 0 3)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06800fb0 0x0ff00ff0 (arm_cond $ liftM3 O_SEL            (arm_r 12 15) (arm_r 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0xf1010000 0xfffffc00 (arm_uncond $ liftM O_SETEND (toEnum . fromIntegral . arm_bit 9))
  ,ARMOpcode32 [ARM_EXT_V6] 0x0700f010 0x0ff0f0d0 (arm_cond $ liftM4 O_SMUAD   (toEnum . fromIntegral . arm_bit 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11)) -- TODO: double check enum direction is correct for first arg
  ,ARMOpcode32 [ARM_EXT_V6] 0x0700f050 0x0ff0f0d0 (arm_cond $ liftM4 O_SMUSD   (toEnum . fromIntegral . arm_bit 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V6] 0x07000010 0x0ff000d0 (arm_cond $ liftM5 O_SMLAD   (toEnum . fromIntegral . arm_bit 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V6] 0x07400010 0x0ff000d0 (arm_cond $ liftM5 O_SMLALD  (toEnum . fromIntegral . arm_bit 5) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V6] 0x07000050 0x0ff000d0 (arm_cond $ liftM5 O_SMLSD   (toEnum . fromIntegral . arm_bit 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x07400050 0x0ff000d0 (arm_cond $ liftM5 O_SMLSLD  (toEnum . fromIntegral . arm_bit 5) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V6] 0x0750f010 0x0ff0f0d0 (arm_cond $ liftM4 O_SMMUL   (arm_bool 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V6] 0x07500010 0x0ff000d0 (arm_cond $ liftM5 O_SMMLA   (arm_bool 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V6] 0x075000d0 0x0ff000d0 (arm_cond $ liftM5 O_SMMLS   (arm_bool 5) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  --,ARMOpcode32 [ARM_EXT_V6] 0xf84d0500 0xfe5fffe0 (arm_uncond $ O_SRS    arm_arr 23 23 ['i' 'd'] arm_arr 24 24 ['b' 'a'] arm_r 16 19 arm_char1 21 21 '!' arm_d 0 4]
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00010 0x0fe00ff0 (arm_cond $ liftM3 O_SSAT    (arm_r 12 15) (arm_W 16 20) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00010 0x0fe00070 (arm_cond $ liftM3 O_SSAT    (arm_r 12 15) (arm_W 16 20) (liftM2 (OP_RegShiftImm S_LSL) (arm_d 7 11) (arm_r 0 3)))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00050 0x0fe00070 (arm_cond $ liftM3 O_SSAT    (arm_r 12 15) (arm_W 16 20) (liftM2 (OP_RegShiftImm S_ASR) (arm_d 7 11) (arm_r 0 3)))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06a00f30 0x0ff00ff0 (arm_cond $ liftM3 O_SSAT16  (arm_r 12 15) (arm_W 16 19) (arm_r 0 3))
  --ARMOpcode32 [ARM_EXT_V6] 0x01800f90 0x0ff00ff0 (arm_cond $ liftM3 O_strex  arm_r 12 15 arm_r 12 15 arm_r 0 3 arm_square (arm_r 16 19)] 
  ,ARMOpcode32 [ARM_EXT_V6] 0x00400090 0x0ff000f0 (arm_cond $ liftM4 O_UMAAL   (arm_r 12 15) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3)) -- (arm_r 8 11)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x0780f010 0x0ff0f0f0 (arm_cond $ liftM3 O_USAD8   (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V6] 0x07800010 0x0ff000f0 (arm_cond $ liftM4 O_USADA8  (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15)) 
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00010 0x0fe00ff0 (arm_cond $ liftM3 O_USAT    (arm_r 12 15) (arm_W 16 20) (OP_Reg . arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00010 0x0fe00070 (arm_cond $ liftM3 O_USAT    (arm_r 12 15) (arm_W 16 20) (liftM2 (OP_RegShiftImm S_LSL) (arm_d 7 11) (arm_r 0 3)))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00050 0x0fe00070 (arm_cond $ liftM3 O_USAT    (arm_r 12 15) (arm_W 16 20) (liftM2 (OP_RegShiftImm S_ASR) (arm_d 7 11) (arm_r 0 3)))
  ,ARMOpcode32 [ARM_EXT_V6] 0x06e00f30 0x0ff00ff0 (arm_cond $ liftM3 O_USAT16  (arm_r 12 15) (arm_W 16 19) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V5J] 0x012fff20 0x0ffffff0 (arm_cond $ liftM O_BXJ     (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V5] 0xe1200070 0xfff000f0 (arm_uncond $ liftM O_BKPT (liftM2 (\x y -> x `shiftL` 4 .|. y) (arm_d 8 19) (arm_d 0 3))) 
  ,ARMOpcode32 [ARM_EXT_V5] 0xfa000000 0xfe000000 (arm_uncond $ liftM O_BLXUC arm_B)
  ,ARMOpcode32 [ARM_EXT_V5] 0x012fff30 0x0ffffff0 (arm_cond $ liftM O_BLX (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V5] 0x016f0f10 0x0fff0ff0 (arm_cond $ liftM2 O_CLZ (arm_r 12 15) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V5E] 0x000000d0 0x0e1000f0 (arm_cond $ liftM2 (O_LDR DoubleWord False False) (arm_r 12 15) arm_s)
  ,ARMOpcode32 [ARM_EXT_V5E] 0x000000f0 0x0e1000f0 (arm_cond $ liftM2 (O_STR DoubleWord False False) (arm_r 12 15) arm_s)
  ,ARMOpcode32 [ARM_EXT_V5E] 0xf450f000 0xfc70f000 (arm_uncond $ liftM O_PLD arm_a)
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01000080 0x0ff000f0 (arm_cond $ liftM4 (O_SMLA B B) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x010000a0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLA T B) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x010000c0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLA B T) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x010000e0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLA T T) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11) (arm_r 12 15))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 (arm_cond $ liftM4 (O_SMLAL $ Just (B, B)) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLAL $ Just (T, B)) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x014000c0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLAL $ Just (B, T)) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x014000e0 0x0ff000f0 (arm_cond $ liftM4 (O_SMLAL $ Just (T, T)) (arm_r 12 15) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01600080 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMUL B B) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x016000a0 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMUL T B) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x016000c0 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMUL B T) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x016000e0 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMUL T T) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x012000a0 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMULW B) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x012000e0 0x0ff0f0f0 (arm_cond $ liftM3 (O_SMULW T) (arm_r 16 19) (arm_r 0 3) (arm_r 8 11))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01000050 0x0ff00ff0 (arm_cond $ liftM3 O_QADD   (arm_r 12 15) (arm_r 0 3) (arm_r 16 19))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01400050 0x0ff00ff0 (arm_cond $ liftM3 O_QDADD  (arm_r 12 15) (arm_r 0 3) (arm_r 16 19))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01200050 0x0ff00ff0 (arm_cond $ liftM3 O_QSUB   (arm_r 12 15) (arm_r 0 3) (arm_r 16 19))
  ,ARMOpcode32 [ARM_EXT_V5ExP] 0x01600050 0x0ff00ff0 (arm_cond $ liftM3 O_QDSUB  (arm_r 12 15) (arm_r 0 3) (arm_r 16 19))
  ,ARMOpcode32 [ARM_EXT_V1] 0x00000090 0x0e100090 (arm_cond $ liftM5 O_STR (arm_arr 5 5 [HalfWord, Byte]) (const False) (arm_bool 6) (arm_r 12 15) arm_s)  --)[arm_const "str", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, arm_r 12 15, arm_s]
  ,ARMOpcode32 [ARM_EXT_V1] 0x00100090 0x0e100090 (arm_cond $ liftM5 O_LDR (arm_arr 5 5 [HalfWord, Byte]) (const False) (arm_bool 6) (arm_r 12 15) arm_s)  --)[arm_const "ldr", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, arm_r 12 15, arm_s]
  ,ARMOpcode32 [ARM_EXT_V1] 0x00000000 0x0de00000 (arm_cond $ liftM4 O_AND (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00200000 0x0de00000 (arm_cond $ liftM4 O_EOR (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00400000 0x0de00000 (arm_cond $ liftM4 O_SUB (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00600000 0x0de00000 (arm_cond $ liftM4 O_RSB (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00800000 0x0de00000 (arm_cond $ liftM4 O_ADD (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00a00000 0x0de00000 (arm_cond $ liftM4 O_ADC (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00c00000 0x0de00000 (arm_cond $ liftM4 O_SBC (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x00e00000 0x0de00000 (arm_cond $ liftM4 O_RSC (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  --,ARMOpcode32 [ARM_EXT_V3] 0x0120f000 0x0db0f000 (arm_cond $ liftM3 O_MSR (arm_arr 22 22 [SPSR, CPSR]) arm_C arm_o)
  --,ARMOpcode32 [ARM_EXT_V3] 0x010f0000 0x0fbf0fff (arm_cond $ liftM2 O_MRS (arm_r 12 15) (arm_arr 22 22 [SPSR, CPSR]))
  ,ARMOpcode32 [ARM_EXT_V1] 0x01000000 0x0de00000 (arm_cond $ liftM2 O_TST {-arm_p-} (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01200000 0x0de00000 (arm_cond $ liftM2 O_TEQ {-arm_p-} (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01400000 0x0de00000 (arm_cond $ liftM2 O_CMP {-arm_p-} (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01600000 0x0de00000 (arm_cond $ liftM2 O_CMN {-arm_p-} (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01800000 0x0de00000 (arm_cond $ liftM4 O_ORR (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)

  ,ARMOpcode32 [ARM_EXT_V1] 0x03a00000 0x0fef0000 (arm_cond $ liftM3 O_MOV (arm_bool 20) (arm_r 12 15) arm_o)
  --,ARMOpcode32 [ARM_EXT_V1] 0x01a00000 0x0def0ff0 (arm_cond $ liftM3 O_MOV (arm_bool 20) (arm_r 12 15) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V1] 0x01a00000 0x0def0060 (arm_cond $ liftM3 O_LSL (arm_bool 20) (arm_r 12 15) arm_q)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01a00020 0x0def0060 (arm_cond $ liftM3 O_LSR (arm_bool 20) (arm_r 12 15) arm_q)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01a00040 0x0def0060 (arm_cond $ liftM3 O_ASR (arm_bool 20) (arm_r 12 15) arm_q)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01a00060 0x0def0ff0 (arm_cond $ liftM3 O_RRX (arm_bool 20) (arm_r 12 15) (arm_r 0 3))
  ,ARMOpcode32 [ARM_EXT_V1] 0x01a00060 0x0def0060 (arm_cond $ liftM3 O_ROR (arm_bool 20) (arm_r 12 15) arm_q)
  
  ,ARMOpcode32 [ARM_EXT_V1] 0x01c00000 0x0de00000 (arm_cond $ liftM4 O_BIC (arm_bool 20) (arm_r 12 15) (arm_r 16 19) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x01e00000 0x0de00000 (arm_cond $ liftM3 O_MVN (arm_bool 20) (arm_r 12 15) arm_o)
  ,ARMOpcode32 [ARM_EXT_V1] 0x052d0004 0x0fff0fff (arm_cond $ liftM2 (O_STR Word False False) (arm_r 12 15) arm_a)
  ,ARMOpcode32 [ARM_EXT_V1] 0x04000000 0x0e100000 (arm_cond $ liftM5 O_STR (arm_bw 22) arm_t (const False) (arm_r 12 15) arm_a)
  ,ARMOpcode32 [ARM_EXT_V1] 0x06000000 0x0e100ff0 (arm_cond $ liftM5 O_STR (arm_bw 22) arm_t (const False) (arm_r 12 15) arm_a)
  ,ARMOpcode32 [ARM_EXT_V1] 0x04000000 0x0c100010 (arm_cond $ liftM5 O_STR (arm_bw 22) arm_t (const False) (arm_r 12 15) arm_a)
  --, ARMOpcode32 [ARM_EXT_V1] 0x06000010 0x0e000010 [arm_const "undefined"]
  ,ARMOpcode32 [ARM_EXT_V1] 0x049d0004 0x0fff0fff (arm_cond $ liftM2 (O_LDR Word False False) (arm_r 12 15) arm_a)
  ,ARMOpcode32 [ARM_EXT_V1] 0x04100000 0x0c100000 (arm_cond $ liftM5 O_LDR (arm_bw 22) arm_t (const False) (arm_r 12 15) arm_a)
  ,ARMOpcode32 [ARM_EXT_V1] 0x092d0000 0x0fff0000 (arm_cond $ liftM O_PUSH arm_m)
  --, ARMOpcode32 [ARM_EXT_V1] 0x08800000 0x0ff00000 [arm_const "stm", arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  --, ARMOpcode32 [ARM_EXT_V1] 0x08000000 0x0e100000 [arm_const "stm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  ,ARMOpcode32 [ARM_EXT_V1] 0x08bd0000 0x0fff0000 (arm_cond $ liftM O_POP arm_m)
  --, ARMOpcode32 [ARM_EXT_V1] 0x08900000 0x0f900000 [arm_const "ldm", arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  --, ARMOpcode32 [ARM_EXT_V1] 0x08100000 0x0e100000 [arm_const "ldm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  ,ARMOpcode32 [ARM_EXT_V1] 0x0a000000 0x0e000000 (arm_cond $ liftM2 O_B (arm_bool 24) arm_b)
  --, ARMOpcode32 [ARM_EXT_V1] 0x0f000000 0x0f000000 [arm_const "svc", arm_c, arm_x 0 23] -- does this belong?
  --, ARMOpcode32 [ARM_EXT_V1] 0x00000000 0x00000000 [arm_const "undefined instruction", arm_x 0 31]  
  ]

armOpcodeMatches :: Word32 -> ARMOpcode32 -> Bool
armOpcodeMatches x (ARMOpcode32 _ v m _) = x .&. m == v 

armDecodeOp :: Word32 -> ARMState -> ARMOpcode32 -> ARMInstruction
armDecodeOp x s (ARMOpcode32 _ _ _ d) = d (s, x)

armDecode :: (Word32, Word32) -> Maybe ARMInstruction
armDecode (a, i) = fmap (armDecodeOp i (ARMState a)) . find (armOpcodeMatches i) $ armOpcodes