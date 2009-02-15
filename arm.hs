-- module ARM where

import qualified Data.ByteString as B

import Data.Binary
import Data.Binary.Get

import Data.Maybe
import Data.List
import Data.Int
import Data.Word
import Data.Bits

import Debug.Trace

data ARMArch = ARM_EXT_V1
             | ARM_EXT_V2
             | ARM_EXT_V2S
             | ARM_EXT_V3
             | ARM_EXT_V3M
             | ARM_EXT_V4T 
             | ARM_EXT_V5
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

data ARMOpcode32 d = ARMOpcode32 { opcode32_arch :: [ARMArch]
                                 , opcode32_value :: Word32
                                 , opcode32_mask :: Word32
                                 , opcode32_decoder :: [d]
                                 }
                               
data ARMOpcode16 d = ARMOpcode16 { opcode16_arch  :: [ARMArch]
                                 , opcode16_value :: Word16
                                 , opcode16_mask :: Word16
                                 , opcode16_decoder :: [d]
                                 }

data ARMState = ARMState { pc :: Word32 }

data ARMRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
                 | R9 | R10 | R11 | R12 | SP | LR | PC
  deriving (Show, Read, Eq, Enum)
  
data ARMShift = S_LSL | S_LSR | S_ASR | S_ROR
  deriving (Show, Read, Eq, Enum)
  
data ARMCondition = C_EQ | C_NE | C_CS | C_CC | C_MI | C_PL | C_VS | C_VC
                  | C_HI | C_LS | C_GE | C_LT | C_GT | C_LE | C_AL | C_UND
  deriving (Show, Read, Eq, Enum)

data Nybble = T | B
 deriving (Show, Read, Eq, Enum)
 
-- FIXME: parametrize more of these
data ARMOpcode = O_B Bool Bool -- B, BL, BX, BLX
               | O_BXJ
               
               | O_AND
               | O_EOR
               | O_SUB
               | O_RSB
               | O_ADD
               | O_ADC
               | O_RSC
               | O_TST
               | O_TEQ
               | O_CMP
               | O_CMN
               | O_ORR
               | O_MOV
               | O_BIC
               | O_MVN
               
               | O_MLA
               | O_MUL
               | O_SMLA Nybble Nybble -- SMLABB, SMLABT, SBMLATB, SMLATT
               | O_SMLAD
               | O_SMLAL (Maybe (Nybble, Nybble))
               | O_SMLALD
               | O_SMLAW Nybble
               | O_SMLSD
               | O_SMLSLD
               | O_SMMLA
               | O_SMMUL
               | O_SMUAD
               | O_SMUL Nybble Nybble
               | O_SMULL
               | O_SMULW Nybble
               | O_SMUSD
               | O_UMAAL
               | O_UMLAL
               | O_UMULL
               
               | O_QADD16
               | O_QADD8
               | O_QADDSUBX
               | O_QSUB16
               | O_QSUB8
               | O_QSUBADDX
               | O_SADD16
               | O_SADD8
               | O_SADDSUBX
               | O_SSUB16
               | O_SSUB8
               | O_SSUBADDX
               | O_SHADD16
               | O_SHADD8
               | O_SHADDSUBX
               | O_SHSUB16
               | O_SHSUB8
               | O_SHSUBADDX
               | O_UADD16
               | O_UADD8
               | O_UADDSUBX
               | O_USUB16
               | O_USUB8
               | O_USUBADDX
               | O_UHADD16
               | O_UHADD8
               | O_UHADDSUBX
               | O_UHSUB16
               | O_UHSUB8
               | O_UHSUBADDX
               | O_UQADD16
               | O_UQADD8
               | O_UQADDSUBX
               | O_UQSUB16
               | O_UQSUB8
               | O_UQSUBADDX
               
               | O_SXTAB16
               | O_SXTAB
               | O_SXTAH
               | O_SXTB16
               | O_SXTB
               | O_SXTH
               | O_UXTAB16
               | O_UXTAB
               | O_UXTAH
               | O_UXTB16
               | O_UXTB
               | O_UXTH
               
               | O_CLZ
               | O_USAD8
               | O_USADA8
               | O_PKHBT
               | O_PKHTB
               | O_REV
               | O_REV16
               | O_REVSH
               | O_SEL
               | O_SSAT
               | O_SSAT16
               | O_USAT
               | O_USAT16
               
               | O_MRS
               | O_MSR
               | O_CPS
               | O_SETEND
               
               | O_LDR
               | O_LDRB
               | O_LDRBT
               | O_LDRD
               | O_LDREX
               | O_LDRH
               | O_LDRSB
               | O_LDRSH
               | O_LDRT
               | O_STR
               | O_STRB
               | O_STRBT
               | O_STRD
               | O_STREX
               | O_STRH
               | O_STRT
               
               | O_LDM -- Note that there are three different forms
               | O_STM -- Note that there are two different forms
               
               | O_SWP
               | O_SWPB
               
               | O_BKPT
               | O_SWI
  deriving (Show, Read, Eq)

data ARMOperand = OP_Imm Int
                
                | OP_Reg ARMRegister
                | OP_RegBang ARMRegister
                
                | OP_RegShiftImm ARMRegister ARMShift Int
                | OP_RegShiftReg ARMRegister ARMShift ARMRegister
                | OP_RegShiftRRX ARMRegister
                
                | OP_MemRegImm ARMRegister Int Bool 
                | OP_MemRegReg ARMRegister ARMRegister Bool
                | OP_MemRegShiftReg ARMRegister ARMRegister ARMShift Int Bool
                | OP_MemRegPostImm ARMRegister Int
                | OP_MemRegPostReg ARMRegister ARMRegister
                | OP_MemRegPostShiftReg ARMRegister ARMRegister ARMShift Int
                
                | OP_Regs [ARMRegister]
                | OP_RegsCaret [ARMRegister]
  deriving (Show, Read, Eq)
                        
data ARMInstruction = ARMInstruction { insn_opcode :: ARMOpcode
                                     , insn_condition :: ARMCondition
                                     , insn_operands :: [ARMOperand]
                                     }
  deriving (Show, Read, Eq)
  
bitRange :: Int -> Int -> Int -> Int
bitRange start end i = ((i `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1))

armConditions = ["eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc", "hi"
                ,"ls", "ge", "lt", "gt", "le", "", "<und>", ""]

armRegisters = ["r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8"
               ,"r9", "r10", "r11", "r12", "sp", "lr", "pc"]

armShift = ["lsl", "lsr", "asr", "ror"]

type ARMDecoder = Word32 -> ARMState -> String

printARMAddress :: Word32 -> ARMState -> String
printARMAddress a s | (a .&. 0xf0000) == 0xf0000 && (a .&. 0x2000000) == 0 = 
                        let offset = a .&. 0xfff in
                          case a .&. 0x1000000 /= 0 of
                            True -> "[pc, #" ++ (show $ if (a .&. 0x800000) == 0 then -offset else offset) ++ (if (a .&. 0x200000) /= 0 then "]!" else "]")
                            _    -> "[pc], " ++ (show offset)
                    | otherwise = 
                          "[" ++ (armRegisters !! (((fromIntegral a) `shiftR` 16 ) .&. 0xf)) ++ case a .&. 0x1000000 /= 0 of
                            False -> if (a .&. 0x2000000) == 0 then
                                       let offset = a .&. 0xfff in
                                         if offset /= 0 then
                                           "], #" ++ if (a .&. 0x800000) == 0 then show (-(fromIntegral offset :: Int32)) else show offset
                                           else "]"
                                       else "], #" ++ (if (a .&. 0x800000) == 0 then "-" else "") ++ (armDecodeShift a False)
                            _     -> if (a .&. 0x2000000) == 0 then
                                       let offset = a .&. 0xfff in
                                         (if offset /= 0 then
                                            ", #" ++ if (a .&. 0x800000) == 0 then show (-(fromIntegral offset :: Int32)) else show offset
                                            else "") ++ if (a .&. 0x200000) /= 0 then "]!" else "]"
                                       else ", #" ++ (if (a .&. 0x800000) == 0 then "-" else "") ++ (armDecodeShift a False)
                                          ++ if (a .&. 0x200000) /= 0 then "]!" else "]"

armDecodeShift :: Word32 -> Bool -> String
armDecodeShift i p = (armRegisters !! ((fromIntegral i) .&. 0xf)) ++
                       if i .&. 0xff0 /= 0 then
                         if i .&. 0x10 == 0 then
                           let amount = (i .&. 0xf80) `shiftR` 7
                               shift = ((fromIntegral i) .&. 0x60) `shiftR` 5 in
                             if amount == 0 && shift == 3 then ", rrx" 
                               else ", " ++ (if p then (armShift !! shift) ++ " " ++ (show amount)
                                               else show amount)
                           else ", " ++ (if p then (armShift !! (((fromIntegral i) .&. 0x60) `shiftR` 5)) ++ " " ++ (armRegisters !! (((fromIntegral i) .&. 0xf00 `shiftR` 8)))
                                           else armRegisters !! (((fromIntegral i) .&. 0xf00 `shiftR` 8)))
                         else ""

arm_const :: String -> ARMDecoder
arm_const x i s = x

arm_constint :: Int -> ARMDecoder
arm_constint x i s = show x

arm_a :: ARMDecoder
arm_a = printARMAddress 

-- FIXME: wow, this is pretty ugly...
arm_s :: ARMDecoder
arm_s i s | i .&. 0x4f0000 == 0x4f0000 = "[pc, #" ++ (show $ (if i .&. 0x800000 == 0 then -1 else 1) * ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf)) ++ "]"
          | i .&. 0x1000000 /= 0 = case i .&. 0x400000 of
              0x400000 -> "[" ++ (armRegisters !! (((fromIntegral i) `shiftR` 16) .&. 0xf)) ++ 
                              (let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                                (if offset /= 0 then if (i .&. 0x800000) == 0 then show (-offset) else show offset else "") 
                                 ++ (if (i .&. 0x200000) /= 0 then "]!" else "]"))
              _        -> "[" ++ (armRegisters !! (((fromIntegral i) `shiftR` 16) .&. 0xf)) ++ 
                              (if (i .&. 0x800000) == 0 then "-" ++ (armRegisters !! ((fromIntegral i) .&. 0xf)) else (armRegisters !! ((fromIntegral i) .&. 0xf))) ++ (if (i .&. 0x200000) /= 0 then "]!" else "]")
          | otherwise = case i .&. 0x400000 of
              0x400000 -> "[" ++ (armRegisters !! (((fromIntegral i) `shiftR` 16) .&. 0xf)) ++ 
                              (let offset = ((i .&. 0xf00) `shiftR` 4) .|. (i .&. 0xf) in 
                                (if offset /= 0 then if (i .&. 0x800000) == 0 then show (-offset) else show offset else "") 
                                 ++ "]")
              _        -> "[" ++ (armRegisters !! (((fromIntegral i) `shiftR` 16) .&. 0xf)) ++ "], " ++ (if (i .&. 0x800000) == 0 then "-" ++ (armRegisters !! ((fromIntegral i) .&. 0xf)) else (armRegisters !! ((fromIntegral i) .&. 0xf)))

arm_b :: ARMDecoder
arm_b i s = show $ ((((fromIntegral i :: Int32) .&. 0xffffff) `xor` 0x800000) - 0x800000) * 4 + (fromIntegral $ pc s) + 8

arm_c :: ARMDecoder
arm_c i s = armConditions !! (((fromIntegral i) `shiftR` 28) .&. 0xf)

arm_m :: ARMDecoder
arm_m i s = "{" ++ (intercalate ", " . filter (not . null) $ map (\x -> if i .&. (1 `shiftL` x) /= 0 then armRegisters !! x else "") [0..15]) ++ "}"

arm_o :: ARMDecoder
arm_o i s | i .&. 0x2000000 /= 0 = show $ (i .&. 0xff) `rotateR` (((fromIntegral i) .&. 0xf00) `shiftR` 7)
          | otherwise = armDecodeShift i True

arm_p :: ARMDecoder
arm_p i s = if (i .&. 0xf000) == 0xf000 then "p" else "" 

arm_t :: ARMDecoder
arm_t i s = if (i .&. 0x1200000) == 0x200000 then "t" else ""

arm_q :: ARMDecoder
arm_q i s = armDecodeShift i False

arm_e :: ARMDecoder
arm_e i s = show $ (i .&. 0xf) .|. ((i .&. 0xfff00) `shiftR` 4)

arm_B :: ARMDecoder
arm_B i s = let offset = ((if i .&. 0x800000 /= 0 then 0xff else 0) + (i .&. 0xffffff)) `shiftL` 2 
                address = offset + (pc s) + 8 + (if i .&. 0x1000000 /= 0 then 2 else 0) in
                  show address
              
-- FIXME: this is ugly
arm_C :: ARMDecoder
arm_C i s = '_' : (if i .&. 0x80000 /= 0 then "f" else "" ++ 
                   if i .&. 0x40000 /= 0 then "s" else "" ++
                   if i .&. 0x20000 /= 0 then "x" else "" ++
                   if i .&. 0x10000 /= 0 then "c" else "")

arm_U :: ARMDecoder
arm_U i s = case i .&. 0xf of
              0xf -> "sy"
              0x7 -> "un"
              0xe -> "st"
              0x6 -> "unst"
              x   -> show x

arm_P :: ARMDecoder
arm_P i s = printARMAddress (i .|. (1 `shiftL` 24)) s

arm_r :: Int -> Int -> ARMDecoder
arm_r start end i s = armRegisters !! (bitRange start end $ fromIntegral i)

arm_d :: Int -> Int -> ARMDecoder
arm_d start end i s = show . bitRange start end $ fromIntegral i

arm_W :: Int -> Int -> ARMDecoder
arm_W start end i s = show . (+1) . bitRange start end $ fromIntegral i

arm_x :: Int -> Int -> ARMDecoder
arm_x = arm_d

arm_X :: Int -> Int -> ARMDecoder
arm_X start end i s = show . (.&. 0xf) . bitRange start end $ fromIntegral i

arm_char1 :: Int -> Int -> Char -> ARMDecoder
arm_char1 start end c i s = if (bitRange start end $ fromIntegral i) == ((2 `shiftL` (end - start)) - 1) then [c] else ""

arm_char0 :: Int -> Int -> Char -> ARMDecoder
arm_char0 start end c i s = if (bitRange start end $ fromIntegral i) == 0 then [c] else ""

arm_arr :: Int -> Int -> [Char] -> ARMDecoder
arm_arr start end c i s = return $ c !! (bitRange start end $ fromIntegral i)

arm_E :: ARMDecoder
arm_E i s = let msb = (i .&. 0x1f0000) `shiftR` 16
                lsb = (i .&. 0xf80) `shiftR` 7
                width = msb - lsb + 1 in
              if width > 0 then
                "#" ++ (show lsb) ++ ", #" ++ (show width)
                else "(invalid " ++ (show lsb) ++ ":" ++ (show msb) ++ ")"            

arm_V :: ARMDecoder
arm_V i s = "#" ++ (show $ (i .&. 0xf0000) `shiftR` 4 .|. (i .&. 0xfff))

arm_square :: ARMDecoder -> ARMDecoder
arm_square d = ((("[" ++) . (++ "]")) .) . d

arm_curly :: ARMDecoder -> ARMDecoder
arm_curly d = ((("{" ++) . (++ "}")) .) . d

arm_lsl :: ARMDecoder -> ARMDecoder
arm_lsl d = (("lsl " ++) .) . d

arm_asr :: ARMDecoder -> ARMDecoder
arm_asr d = (("asr " ++) .) . d

arm_ror :: ARMDecoder -> ARMDecoder
arm_ror d = (("ror " ++) .) . d
                              
armOpcodes = 
  [ ARMOpcode32 [ARM_EXT_V1] 0xe1a00000 0xffffffff [arm_const "nop"]
  , ARMOpcode32 [ARM_EXT_V4T, ARM_EXT_V5] 0x012FFF10 0x0ffffff0 [arm_const "bx", arm_c, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V2] 0x00000090 0x0fe000f0 [arm_const "mul", arm_char1 20 20 's', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V2] 0x00200090 0x0fe000f0 [arm_const "mla", arm_char1 20 20 's', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15]
  , ARMOpcode32 [ARM_EXT_V2S] 0x01000090 0x0fb00ff0 [arm_const "swp", arm_char1 22 22 'b', arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)]
  , ARMOpcode32 [ARM_EXT_V3M] 0x00800090 0x0fa000f0 [arm_arr 22 22 ['s', 'u'], arm_const "mull", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V3M] 0x00800090 0x0fa000f0 [arm_arr 22 22 ['s', 'u'], arm_const "mlal", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V7] 0xf450f000 0xfd70f000 [arm_const "pli", arm_P]
  , ARMOpcode32 [ARM_EXT_V7] 0x0320f0f0 0x0ffffff0 [arm_const "dbg", arm_c, arm_d 0 3]
  , ARMOpcode32 [ARM_EXT_V7] 0xf57ff050 0x0ffffff0 [arm_const "dmb", arm_U]
  , ARMOpcode32 [ARM_EXT_V7] 0xf57ff040 0x0ffffff0 [arm_const "dsb", arm_U]
  , ARMOpcode32 [ARM_EXT_V7] 0xf57ff060 0x0ffffff0 [arm_const "isb", arm_U]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x07c0001f 0x0fe0007f [arm_const "bfc", arm_c, arm_r 12 15, arm_E] 
  , ARMOpcode32 [ARM_EXT_V6T2] 0x07c00010 0x0fe00070 [arm_const "bfi", arm_c, arm_r 12 15, arm_r 0 3, arm_E]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x00600090 0x0ff000f0 [arm_const "mls", arm_c, arm_r 0 3, arm_r 8 11, arm_r 12 15]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x006000b0 0x0f7000f0 [arm_const "strht", arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x00300090 0x0f300090 [arm_const "ldr", arm_char1 6 6 's', arm_arr 5 5 ['h','b'], arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x03000000 0x0ff00000 [arm_const "movw", arm_c, arm_r 12 15, arm_V]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x03400000 0x0ff00000 [arm_const "movt", arm_c, arm_r 12 15, arm_V]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x06ff0f30 0x0fff0ff0 [arm_const "rbit", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6T2] 0x07a00050 0x0fa00070 [arm_arr 22 22 ['u', 's'], arm_const "bfx", arm_c, arm_r 12 15, arm_r 0 3, arm_d 7 11, arm_W 16 20]
  , ARMOpcode32 [ARM_EXT_V6Z] 0x01600070 0x0ff000f0 [arm_const "smc", arm_c, arm_e]
  , ARMOpcode32 [ARM_EXT_V6K] 0xf57ff01f 0xffffffff [arm_const "clrex"]
  , ARMOpcode32 [ARM_EXT_V6K] 0x01d00f9f 0x0ff00fff [arm_const "ldrexb", arm_c, arm_r 12 15, arm_square (arm_r 16 19)]
  , ARMOpcode32 [ARM_EXT_V6K] 0x01b00f9f 0x0ff00fff [arm_const "ldrexd", arm_c, arm_r 12 15, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6K] 0x01f00f9f 0x0ff00fff [arm_const "ldrexh", arm_c, arm_r 12 15, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6K] 0x01c00f90 0x0ff00ff0 [arm_const "strexb", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)]
  , ARMOpcode32 [ARM_EXT_V6K] 0x01a00f90 0x0ff00ff0 [arm_const "strexd", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6K] 0x01e00f90 0x0ff00ff0 [arm_const "strexh", arm_c, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6K] 0x0320f001 0x0fffffff [arm_const "yield", arm_c]
  , ARMOpcode32 [ARM_EXT_V6K] 0x0320f002 0x0fffffff [arm_const "wfe", arm_c] 
  , ARMOpcode32 [ARM_EXT_V6K] 0x0320f003 0x0fffffff [arm_const "wfi", arm_c]
  , ARMOpcode32 [ARM_EXT_V6K] 0x0320f004 0x0fffffff [arm_const "sev", arm_c]
  , ARMOpcode32 [ARM_EXT_V6K] 0x0320f000 0x0fffff00 [arm_const "nop", arm_c, arm_curly (arm_d 0 7)]
  , ARMOpcode32 [ARM_EXT_V6] 0xf1080000 0xfffffe3f [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f']
  , ARMOpcode32 [ARM_EXT_V6] 0xf10a0000 0xfffffe20 [arm_const "cpsie", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
  , ARMOpcode32 [ARM_EXT_V6] 0xf10C0000 0xfffffe3f [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f'] 
  , ARMOpcode32 [ARM_EXT_V6] 0xf10e0000 0xfffffe20 [arm_const "cpsid", arm_char1 8 8 'a', arm_char1 7 7 'i', arm_char1 6 6 'f', arm_d 0 4] 
  , ARMOpcode32 [ARM_EXT_V6] 0xf1000000 0xfff1fe20 [arm_const "cps", arm_d 0 4]
  , ARMOpcode32 [ARM_EXT_V6] 0x06800010 0x0ff00ff0 [arm_const "pkhbt", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06800010 0x0ff00070 [arm_const "pkhbt", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_lsl (arm_d 7 11)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06800050 0x0ff00ff0 [arm_const "pkhtb", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_asr (arm_constint 32)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06800050 0x0ff00070 [arm_const "pkhtb", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_asr (arm_d 7 11)]
  , ARMOpcode32 [ARM_EXT_V6] 0x01900f9f 0x0ff00fff [arm_const "ldrex", arm_c, arm_d 12 15, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200f10 0x0ff00ff0 [arm_const "qadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200f90 0x0ff00ff0 [arm_const "qadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200f30 0x0ff00ff0 [arm_const "qaddsubx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200f70 0x0ff00ff0 [arm_const "qsub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200ff0 0x0ff00ff0 [arm_const "qsub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06200f50 0x0ff00ff0 [arm_const "qsubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100f10 0x0ff00ff0 [arm_const "sadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100f90 0x0ff00ff0 [arm_const "sadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100f30 0x0ff00ff0 [arm_const "saddaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300f10 0x0ff00ff0 [arm_const "shadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300f90 0x0ff00ff0 [arm_const "shadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300f30 0x0ff00ff0 [arm_const "shaddsubx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300f70 0x0ff00ff0 [arm_const "shsub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300ff0 0x0ff00ff0 [arm_const "shsub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06300f50 0x0ff00ff0 [arm_const "shsubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100f70 0x0ff00ff0 [arm_const "ssub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100ff0 0x0ff00ff0 [arm_const "ssub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06100f50 0x0ff00ff0 [arm_const "ssubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500f10 0x0ff00ff0 [arm_const "uadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500f90 0x0ff00ff0 [arm_const "uadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500f30 0x0ff00ff0 [arm_const "uaddsubx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700f10 0x0ff00ff0 [arm_const "uhadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700f90 0x0ff00ff0 [arm_const "uhadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700f30 0x0ff00ff0 [arm_const "uhaddsubx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700f70 0x0ff00ff0 [arm_const "uhsub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700ff0 0x0ff00ff0 [arm_const "uhsub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06700f50 0x0ff00ff0 [arm_const "uhsubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600f10 0x0ff00ff0 [arm_const "uqadd16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600f90 0x0ff00ff0 [arm_const "uqadd8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600f30 0x0ff00ff0 [arm_const "uqaddsubx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600f70 0x0ff00ff0 [arm_const "uqsub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600ff0 0x0ff00ff0 [arm_const "uqsub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06600f50 0x0ff00ff0 [arm_const "uqsubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500f70 0x0ff00ff0 [arm_const "usub16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500ff0 0x0ff00ff0 [arm_const "usub8", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06500f50 0x0ff00ff0 [arm_const "usubaddx", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0f30 0x0fff0ff0 [arm_const "rev", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0fb0 0x0fff0ff0 [arm_const "rev16", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ff0fb0 0x0fff0ff0 [arm_const "revsh", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0xf8100a00 0xfe50ffff [arm_const "rfe", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_r 16 19, arm_char1 21 21 '!']
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0070 0x0fff0ff0 [arm_const "sxth", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0470 0x0fff0ff0 [arm_const "sxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0870 0x0fff0ff0 [arm_const "sxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06bf0c70 0x0fff0ff0 [arm_const "sxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x068f0070 0x0fff0ff0 [arm_const "sxtb16", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x068f0470 0x0fff0ff0 [arm_const "sxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x068f0870 0x0fff0ff0 [arm_const "sxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x068f0c70 0x0fff0ff0 [arm_const "sxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06af0070 0x0fff0ff0 [arm_const "sxtb", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06af0470 0x0fff0ff0 [arm_const "sxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06af0870 0x0fff0ff0 [arm_const "sxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06af0c70 0x0fff0ff0 [arm_const "sxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ff0070 0x0fff0ff0 [arm_const "uxth", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ff0470 0x0fff0ff0 [arm_const "uxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06ff0870 0x0fff0ff0 [arm_const "uxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ff0c70 0x0fff0ff0 [arm_const "uxth", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06cf0070 0x0fff0ff0 [arm_const "uxtb16", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06cf0470 0x0fff0ff0 [arm_const "uxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06cf0870 0x0fff0ff0 [arm_const "uxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06cf0c70 0x0fff0ff0 [arm_const "uxtb16", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ef0070 0x0fff0ff0 [arm_const "uxtb", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ef0470 0x0fff0ff0 [arm_const "uxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 8)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06ef0870 0x0fff0ff0 [arm_const "uxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 16)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06ef0c70 0x0fff0ff0 [arm_const "uxtb", arm_c, arm_r 12 15, arm_r 0 3, arm_ror (arm_constint 24)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06b00070 0x0ff00ff0 [arm_const "sxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06b00470 0x0ff00ff0 [arm_const "sxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )]
  , ARMOpcode32 [ARM_EXT_V6] 0x06b00870 0x0ff00ff0 [arm_const "sxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06b00c70 0x0ff00ff0 [arm_const "sxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06800070 0x0ff00ff0 [arm_const "sxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06800470 0x0ff00ff0 [arm_const "sxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06800870 0x0ff00ff0 [arm_const "sxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06800c70 0x0ff00ff0 [arm_const "sxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00070 0x0ff00ff0 [arm_const "sxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00470 0x0ff00ff0 [arm_const "sxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00870 0x0ff00ff0 [arm_const "sxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00c70 0x0ff00ff0 [arm_const "sxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06f00070 0x0ff00ff0 [arm_const "uxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06f00470 0x0ff00ff0 [arm_const "uxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06f00870 0x0ff00ff0 [arm_const "uxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06f00c70 0x0ff00ff0 [arm_const "uxtah", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06c00070 0x0ff00ff0 [arm_const "uxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06c00470 0x0ff00ff0 [arm_const "uxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06c00870 0x0ff00ff0 [arm_const "uxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06c00c70 0x0ff00ff0 [arm_const "uxtab16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00070 0x0ff00ff0 [arm_const "uxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00470 0x0ff00ff0 [arm_const "uxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 8 )] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00870 0x0ff00ff0 [arm_const "uxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 16)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00c70 0x0ff00ff0 [arm_const "uxtab", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_ror (arm_constint 24)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06800fb0 0x0ff00ff0 [arm_const "sel", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0xf1010000 0xfffffc00 [arm_const "setend", arm_arr 9 9 ['b', 'l'], arm_const "e"]
  , ARMOpcode32 [ARM_EXT_V6] 0x0700f010 0x0ff0f0d0 [arm_const "smuad", arm_char1 5 5 'x', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V6] 0x0700f050 0x0ff0f0d0 [arm_const "smusd", arm_char1 5 5 'x', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V6] 0x07000010 0x0ff000d0 [arm_const "smlad", arm_char1 5 5 'x', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15] 
  , ARMOpcode32 [ARM_EXT_V6] 0x07400010 0x0ff000d0 [arm_const "smlald", arm_char1 5 5 'x', arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V6] 0x07000050 0x0ff000d0 [arm_const "smlsd", arm_char1 5 5 'x', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15] 
  , ARMOpcode32 [ARM_EXT_V6] 0x07400050 0x0ff000d0 [arm_const "smlsld", arm_char1 5 5 'x', arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V6] 0x0750f010 0x0ff0f0d0 [arm_const "smmul", arm_char1 5 5 'r', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V6] 0x07500010 0x0ff000d0 [arm_const "smmla", arm_char1 5 5 'r', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15] 
  , ARMOpcode32 [ARM_EXT_V6] 0x075000d0 0x0ff000d0 [arm_const "smmls", arm_char1 5 5 'r', arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15] 
  , ARMOpcode32 [ARM_EXT_V6] 0xf84d0500 0xfe5fffe0 [arm_const "srs", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_r 16 19, arm_char1 21 21 '!', arm_d 0 4]
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00010 0x0fe00ff0 [arm_const "ssat", arm_c, arm_r 12 15, arm_W 16 20, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00010 0x0fe00070 [arm_const "ssat", arm_c, arm_r 12 15, arm_W 16 20, arm_r 0 3, arm_lsl (arm_d 7 11)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00050 0x0fe00070 [arm_const "ssat", arm_c, arm_r 12 15, arm_W 16 20, arm_r 0 3, arm_asr (arm_d 7 11)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06a00f30 0x0ff00ff0 [arm_const "ssat16", arm_c, arm_r 12 15, arm_W 16 19, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x01800f90 0x0ff00ff0 [arm_const "strex", arm_c, arm_r 12 15, arm_r 12 15, arm_r 0 3, arm_square (arm_r 16 19)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x00400090 0x0ff000f0 [arm_const "umaal", arm_c, arm_r 12 15, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V6] 0x0780f010 0x0ff0f0f0 [arm_const "usad8", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V6] 0x07800010 0x0ff000f0 [arm_const "usada8", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00010 0x0fe00ff0 [arm_const "usat", arm_c, arm_r 12 15, arm_r 16 20, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00010 0x0fe00070 [arm_const "usat", arm_c, arm_r 12 15, arm_r 16 20, arm_r 0 3, arm_lsl (arm_d 7 11)]
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00050 0x0fe00070 [arm_const "usat", arm_c, arm_r 12 15, arm_r 16 20, arm_r 0 3, arm_asr (arm_d 7 11)] 
  , ARMOpcode32 [ARM_EXT_V6] 0x06e00f30 0x0ff00ff0 [arm_const "usat16", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V5J] 0x012fff20 0x0ffffff0 [arm_const "bxj", arm_c, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V5] 0xe1200070 0xfff000f0 [arm_const "bkpt", arm_X 16 19, arm_X 12 15, arm_X 8 11, arm_X 0 3] -- compound number
  , ARMOpcode32 [ARM_EXT_V5] 0xfa000000 0xfe000000 [arm_const "blx", arm_B]
  , ARMOpcode32 [ARM_EXT_V5] 0x012fff30 0x0ffffff0 [arm_const "blx", arm_c, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V5] 0x016f0f10 0x0fff0ff0 [arm_const "clz", arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V5E] 0x000000d0 0x0e1000f0   [arm_const "ldrd", arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V5E] 0x000000f0 0x0e1000f0   [arm_const "strd", arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V5E] 0xf450f000 0xfc70f000   [arm_const "pld", arm_a]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01000080 0x0ff000f0 [arm_const "smlabb", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000a0 0x0ff000f0 [arm_const "smlatb", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000c0 0x0ff000f0 [arm_const "smlabt", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x010000e0 0x0ff000f0 [arm_const "smlatt", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11, arm_r 12 15]  
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01400080 0x0ff000f0 [arm_const "smlalbb", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000a0 0x0ff000f0 [arm_const "smlaltb", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000c0 0x0ff000f0 [arm_const "smlalbt", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x014000e0 0x0ff000f0 [arm_const "smlaltt", arm_c, arm_r 12 15, arm_r 16 19, arm_r 0 3, arm_r 8 11] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01600080 0x0ff0f0f0 [arm_const "smulbb", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000a0 0x0ff0f0f0 [arm_const "smultb", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000c0 0x0ff0f0f0 [arm_const "smulbt", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x016000e0 0x0ff0f0f0 [arm_const "smultt", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x012000a0 0x0ff0f0f0 [arm_const "smulwb", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x012000e0 0x0ff0f0f0 [arm_const "smulwt", arm_c, arm_r 16 19, arm_r 0 3, arm_r 8 11]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01000050 0x0ff00ff0 [arm_const "qadd", arm_c, arm_r 12 15, arm_r 0 3, arm_r 16 19] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01400050 0x0ff00ff0 [arm_const "qdadd", arm_c, arm_r 12 15, arm_r 0 3, arm_r 16 19]
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01200050 0x0ff00ff0 [arm_const "qsub", arm_c, arm_r 12 15, arm_r 0 3, arm_r 16 19] 
  , ARMOpcode32 [ARM_EXT_V5ExP] 0x01600050 0x0ff00ff0 [arm_const "qdsub", arm_c, arm_r 12 15, arm_r 0 3, arm_r 16 19]
  , ARMOpcode32 [ARM_EXT_V1] 0x00000090 0x0e100090 [arm_const "str", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V1] 0x00100090 0x0e100090 [arm_const "ldr", arm_char1 6 6 's', arm_arr 5 5 ['h', 'b'], arm_c, arm_r 12 15, arm_s]
  , ARMOpcode32 [ARM_EXT_V1] 0x00000000 0x0de00000 [arm_const "and", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00200000 0x0de00000 [arm_const "eor", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00400000 0x0de00000 [arm_const "sub", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00600000 0x0de00000 [arm_const "rsb", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00800000 0x0de00000 [arm_const "add", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00a00000 0x0de00000 [arm_const "adc", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00c00000 0x0de00000 [arm_const "sbc", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x00e00000 0x0de00000 [arm_const "rsc", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V3] 0x0120f000 0x0db0f000 [arm_const "msr", arm_c, arm_arr 22 22 ['S', 'C'], arm_const "PSR", arm_C, arm_o]
  , ARMOpcode32 [ARM_EXT_V3] 0x010f0000 0x0fbf0fff [arm_const "mrs", arm_c, arm_r 12 15, arm_arr 22 22 ['S', 'C'], arm_const "PSR"]
  , ARMOpcode32 [ARM_EXT_V1] 0x01000000 0x0de00000 [arm_const "tst", arm_p, arm_c, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01200000 0x0de00000 [arm_const "teq", arm_p, arm_c, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01400000 0x0de00000 [arm_const "cmp", arm_p, arm_c, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01600000 0x0de00000 [arm_const "cmn", arm_p, arm_c, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01800000 0x0de00000 [arm_const "orr", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x03a00000 0x0fef0000 [arm_const "mov", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00000 0x0def0ff0 [arm_const "mov", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00000 0x0def0060 [arm_const "lsl", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_q]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00020 0x0def0060 [arm_const "lsr", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_q]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00040 0x0def0060 [arm_const "asr", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_q]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00060 0x0def0ff0 [arm_const "rrx", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 0 3]
  , ARMOpcode32 [ARM_EXT_V1] 0x01a00060 0x0def0060 [arm_const "ror", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_q]
  , ARMOpcode32 [ARM_EXT_V1] 0x01c00000 0x0de00000 [arm_const "bic", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_r 16 19, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x01e00000 0x0de00000 [arm_const "mvn", arm_char1 20 20 's', arm_c, arm_r 12 15, arm_o]
  , ARMOpcode32 [ARM_EXT_V1] 0x052d0004 0x0fff0fff [arm_const "str", arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x04000000 0x0e100000 [arm_const "str", arm_char1 22 22 'b', arm_t, arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x06000000 0x0e100ff0 [arm_const "str", arm_char1 22 22 'b', arm_t, arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x04000000 0x0c100010 [arm_const "str", arm_char1 22 22 'b', arm_t, arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x06000010 0x0e000010 [arm_const "undefined"]
  , ARMOpcode32 [ARM_EXT_V1] 0x049d0004 0x0fff0fff [arm_const "ldr", arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x04100000 0x0c100000 [arm_const "ldr", arm_char1 22 22 'b', arm_t, arm_c, arm_r 12 15, arm_a]
  , ARMOpcode32 [ARM_EXT_V1] 0x092d0000 0x0fff0000 [arm_const "push", arm_c, arm_m]
  , ARMOpcode32 [ARM_EXT_V1] 0x08800000 0x0ff00000 [arm_const "stm", arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1] 0x08000000 0x0e100000 [arm_const "stm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1] 0x08bd0000 0x0fff0000 [arm_const "pop", arm_c, arm_m]
  , ARMOpcode32 [ARM_EXT_V1] 0x08900000 0x0f900000 [arm_const "ldm", arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1] 0x08100000 0x0e100000 [arm_const "ldm", arm_arr 23 23 ['i', 'd'], arm_arr 24 24 ['b', 'a'], arm_c, arm_r 16 19, arm_char1 21 21 '!', arm_m, arm_char1 22 22 '^']
  , ARMOpcode32 [ARM_EXT_V1] 0x0a000000 0x0e000000 [arm_const "b", arm_char1 24 24 'l', arm_c, arm_b]
  , ARMOpcode32 [ARM_EXT_V1] 0x0f000000 0x0f000000 [arm_const "svc", arm_c, arm_x 0 23] -- does this belong?
  , ARMOpcode32 [ARM_EXT_V1] 0x00000000 0x00000000 [arm_const "undefined instruction", arm_x 0 31]
  ]

armOpcodeMatches :: Word32 -> (ARMOpcode32 ARMDecoder) -> Bool
armOpcodeMatches x (ARMOpcode32 _ v m _) = x .&. m == v 

armDecodeOp :: Word32 -> ARMState -> (ARMOpcode32 ARMDecoder) -> [String]
armDecodeOp x s (ARMOpcode32 _ _ _ ds) = map (\f -> f x s) ds

armDecode :: (Word32, Word32) -> [String]
armDecode (a, i) = armDecodeOp i (ARMState a) . fromJust . find (armOpcodeMatches i) $ armOpcodes
  
main = mapM_ print . map armDecode $
                             zip [0x2000,0x2004..] [0xE59D0000,
                                                    0xE28D1004,
                                                    0xE2804001,
                                                    0xE0812104,
                                                    0xE3CDD007,
                                                    0xE1A03002,
                                                    0xE4934004,
                                                    0xE3540000,
                                                    0x1AFFFFFC,
                                                    0xE59FC018,
                                                    0xE08FC00C,
                                                    0xE59CC000,
                                                    0xE12FFF3C,
                                                    0xE59FC00C,
                                                    0xE08FC00C,
                                                    0xE59CC000,
                                                    0xE12FFF1C,
                                                    0x000B2FD0,
                                                    0x000B2FC4,
                                                    0xE52DC004,
                                                    0xE59FC00C,
                                                    0xE79FC00C,
                                                    0xE52DC004,
                                                    0xE59FC004,
                                                    0xE79FF00C,
                                                    0x000B3748,
                                                    0x000B38F0,
                                                    0xE59FC000,
                                                    0xE79FF00C,
                                                    0x000B38E4,
                                                    0xE92D4080,
                                                    0xE28D7000,
                                                    0xE24DD004,
                                                    0xE58D0000,
                                                    0xE59D2000,
                                                    0xE3A03001,
                                                    0xE5823000,
                                                    0xE247D000,
                                                    0xE8BD8080,
                                                    0xE92D4080,
                                                    0xE28D7000,
                                                    0xE24DD008,
                                                    0xE58D0000,
                                                    0xE59D3000,
                                                    0xE58D3004,
                                                    0xE59D3004,
                                                    0xE593C004,
                                                    0xE59F3030,
                                                    0xE08F3003,
                                                    0xE1A00003,
                                                    0xE59D1000,
                                                    0xE59D2000,
                                                    0xE12FFF3C,
                                                    0xE1A03000,
                                                    0xE3530000,
                                                    0x0A000002,
                                                    0xE59D2004,
                                                    0xE3E03000,
                                                    0xE5823000,
                                                    0xE247D000,
                                                    0xE8BD8080,
                                                    0xFFFFFFB0,
                                                    0xE92D4090,
                                                    0xE28D7004,
                                                    0xE24DD014,
                                                    0xE58D0008,
                                                    0xE58D1004,
                                                    0xE3A03000,
                                                    0xE58D300C,
                                                    0xE59D3008,
                                                    0xE58D3010,
                                                    0xE28D200C,
                                                    0xE28DC00C,
                                                    0xE59D4008,
                                                    0xE59F3074,
                                                    0xE08F3003,
                                                    0xE1A00003,
                                                    0xE1A01002,
                                                    0xE1A0200C,
                                                    0xE12FFF34,
                                                    0xE1A03000,
                                                    0xE3530000,
                                                    0x0A000002,
                                                    0xE3E03000,
                                                    0xE58D3000,
                                                    0xEA00000E,
                                                    0xE28D300C,
                                                    0xE59D2004,
                                                    0xE1A00003,
                                                    0xE12FFF32,
                                                    0xE59D300C,
                                                    0xE3530000] -- -}