{-# Language MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Architecture.ARM.Decoder.Thumb where

import Prelude hiding (and)

import Architecture.ARM.Common
import Architecture.ARM.Instructions.UAL

import Data.Maybe
import Data.List hiding (and)
import Data.Int
import Data.Word hiding (Word)
import Data.Bits hiding (bit, shift)

import Text.Printf

import Control.Monad
import Control.Applicative

data Thumb = Thumb
type instance Word Thumb = Word16

instance Decoder Thumb (Instruction UAL Conditional) where
  type Target Thumb (Instruction UAL Conditional) = GeneralInstruction UAL

  decoder s v m d = GeneralDecoder s v m (Conditional AL <$> d) 

instance Decoder Thumb (Instruction UAL Unconditional) where
  type Target Thumb (Instruction UAL Unconditional) = GeneralInstruction UAL
  
  decoder s v m d = GeneralDecoder s v m (Unconditional <$> d)



type D a = Word16 -> a


bitRange :: (Integral a, Bits a, Integral b) => Int -> Int -> a -> b
bitRange start end i = fromIntegral ((((fromIntegral i :: Integer) `shiftR` start) .&. ((2 `shiftL` (end - start)) - 1)))

allSet :: Int -> Int -> D Bool
allSet start end = (== (((2 :: Word32) `shiftL` (end - start)) - 1)) . bitRange start end

noneSet :: Int -> Int -> D Bool
noneSet start end = (== 0) . bitRange start end

integral :: (Integral a, Bits a) => Int -> Int -> D a
integral start end i = bitRange start end i

bit b i = bitRange b b i

bool b s = bit b s == 1

reg :: Int -> D Register
reg start = toEnum . bitRange start (start + 2)

reg4 :: Int -> D Register
reg4 start = toEnum . bitRange start (start + 3)

branch :: D Word32
branch = liftA2 (.|.) ((`shiftL` 1) . integral 3 7) ((`shiftL` 6) . integral 9 9)

choose :: Int -> a -> a -> D a
choose n t f x = if not (bool n x) then t else f

select :: [a] -> [Bool] -> [a]
select = ((map fst . filter snd) .) . zip

bits :: Int -> Int -> D [Bool]
bits = (mapM bool .) . enumFromTo

registers = [R0 ..]

pushRegs = 
  do regBits <- bits 0 7
     Regs <$> choose 8 (select registers regBits) (select registers regBits ++ [LR])

popRegs = 
  do regBits <- bits 0 7
     Regs <$> choose 8 (select registers regBits) (select registers regBits ++ [PC])

shift :: D Int32
shift = 
  do x <- integral 6 10
     if x == 0
       then return 32
       else return x

condition :: D Condition
condition = toEnum . integral 8 11

ifthen :: D ITSpecifier
ifthen = 
  do bs <- mapM bool [4,3..0]
     case bs of
       [fc, True, False, False, False] -> return Nil
       [fc, m3  , True , False, False] | m3 == fc -> return T
       [fc, m3  , True , False, False] | m3 == fc -> return E
       [fc, m3  , m2   , True , False] | m3 == fc && m2 == fc -> return TT
       [fc, m3  , m2   , True , False] | m3 /= fc && m2 == fc -> return ET
       [fc, m3  , m2   , True , False] | m3 == fc && m2 /= fc -> return TE
       [fc, m3  , m2   , True , False] | m3 /= fc && m2 /= fc -> return EE
       [fc, m3  , m2   , m1   , True ] | m3 == fc && m2 == fc && m1 == fc -> return TTT
       [fc, m3  , m2   , m1   , True ] | m3 /= fc && m2 == fc && m1 == fc -> return ETT
       [fc, m3  , m2   , m1   , True ] | m3 == fc && m2 /= fc && m1 == fc -> return TET
       [fc, m3  , m2   , m1   , True ] | m3 /= fc && m2 /= fc && m1 == fc -> return EET
       [fc, m3  , m2   , m1   , True ] | m3 == fc && m2 == fc && m1 /= fc -> return TTE
       [fc, m3  , m2   , m1   , True ] | m3 /= fc && m2 == fc && m1 /= fc -> return ETE
       [fc, m3  , m2   , m1   , True ] | m3 == fc && m2 /= fc && m1 /= fc -> return TEE
       [fc, m3  , m2   , m1   , True ] | m3 /= fc && m2 /= fc && m1 /= fc -> return EEE
       _ -> return Nil -- Eww


thumbDecoders :: [GeneralDecoder Thumb (GeneralInstruction UAL)]
thumbDecoders = 
  [ decoder [ARM_EXT_V6K]  0xbf00 0xffff (pure NOP)
  , decoder [ARM_EXT_V6K]  0xbf10 0xffff (pure YIELD)
  , decoder [ARM_EXT_V6K]  0xbf20 0xffff (pure WFE)
  , decoder [ARM_EXT_V6K]  0xbf30 0xffff (pure WFI)
  , decoder [ARM_EXT_V6K]  0xbf40 0xffff (pure SEV)
  
  , decoder [ARM_EXT_V6T2] 0xb900 0xfd00 (CBNZ <$> reg 0 <*> branch) -- "cbnz\t%0-2r, %b%X"},
  , decoder [ARM_EXT_V6T2] 0xb100 0xfd00 (CBZ  <$> reg 0 <*> branch) -- "cbz\t%0-2r, %b%X"},
  , decoder [ARM_EXT_V6T2] 0xbf00 0xff00 (IT   <$> ifthen) -- "it%I%X"},
  
  , decoder [ARM_EXT_V6]   0xb660 0xfff8 (CPSIE  <$> bool 2 <*> bool 1 <*> bool 0 <*> pure Nothing) -- "cpsie\t%2'a%1'i%0'f%X"},
  , decoder [ARM_EXT_V6]   0xb670 0xfff8 (CPSID  <$> bool 2 <*> bool 1 <*> bool 0 <*> pure Nothing) -- "cpsid\t%2'a%1'i%0'f%X"},
  , decoder [ARM_EXT_V6]   0x4600 0xffc0 (MOV    <$> reg 0 <*> (Reg <$> reg 3)) -- "mov%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xba00 0xffc0 (REV    <$> reg 0 <*> reg 3) -- "rev%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xba40 0xffc0 (REV16  <$> reg 0 <*> reg 3) -- "rev16%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xbac0 0xffc0 (REVSH  <$> reg 0 <*> reg 3) -- "revsh%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xb650 0xfff7 (SETEND <$> choose 3 Big Little) -- "setend\t%3?ble%X"},
  , decoder [ARM_EXT_V6]   0xb200 0xffc0 (SXTH   <$> reg 0 <*> (Reg <$> reg 3)) -- "sxth%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xb240 0xffc0 (SXTB   <$> reg 0 <*> (Reg <$> reg 3)) -- "sxtb%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xb280 0xffc0 (UXTH   <$> reg 0 <*> (Reg <$> reg 3)) -- "uxth%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V6]   0xb2c0 0xffc0 (UXTB   <$> reg 0 <*> (Reg <$> reg 3)) -- "uxtb%c\t%0-2r, %3-5r"},
  
  , decoder [ARM_EXT_V5T]  0xbe00 0xff00 (BKPT <$> integral 0 8) -- "bkpt\t%0-7x"}, /* Is always unconditional.  */
  
  , decoder [ARM_EXT_V5T]  0x4780 0xff87 (blxu <$> (Reg <$> reg4 3)) -- "blx%c\t%3-6r%x"},	/* note: 4 bit register number.  */
  
  , decoder [ARM_EXT_V4T]  0x46C0 0xFFFF (pure NOP) -- "nop%c\t\t\t; (mov r8, r8)"},

  , decoder [ARM_EXT_V4T]  0x4000 0xFFC0 (ANDS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "and%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4040 0xFFC0 (EORS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "eor%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4080 0xFFC0 (LSLS <$> reg 0 <*> (Reg <$> reg 3)) -- "lsl%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x40C0 0xFFC0 (LSRS <$> reg 0 <*> (Reg <$> reg 3)) -- "lsr%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4100 0xFFC0 (ASRS <$> reg 0 <*> (Reg <$> reg 3)) -- "asr%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4140 0xFFC0 (ADCS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "adc%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4180 0xFFC0 (SBCS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "sbc%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x41C0 0xFFC0 (RORS <$> reg 0 <*> (Reg <$> reg 3)) -- "ror%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4200 0xFFC0 (TST  <$> reg 0 <*> (Reg <$> reg 3)) -- "tst%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4240 0xFFC0 (RSBS <$> reg 0 <*> reg 3 <*> pure (Imm 0)) -- "neg%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4280 0xFFC0 (CMP  <$> reg 0 <*> (Reg <$> reg 3)) -- "cmp%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x42C0 0xFFC0 (CMN  <$> reg 0 <*> (Reg <$> reg 3)) -- "cmn%c\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4300 0xFFC0 (ORRS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "orr%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4340 0xFFC0 (MULS <$> reg 0 <*> reg 0 <*> reg 3) -- "mul%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x4380 0xFFC0 (BICS <$> reg 0 <*> reg 0 <*> (Reg <$> reg 3)) -- "bic%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x43C0 0xFFC0 (MVNS <$> reg 0 <*> (Reg <$> reg 3)) -- "mvn%C\t%0-2r, %3-5r"},
  
  , decoder [ARM_EXT_V4T]  0xB000 0xFF80 (ADD SP <$> pure SP <*> (Imm <$> ((`shiftL` 2) . integral 0 6))) -- "add%c\tsp, #%0-6W"},
  , decoder [ARM_EXT_V4T]  0xB080 0xFF80 (SUB SP <$> pure SP <*> (Imm <$> ((`shiftL` 2) . integral 0 6))) -- "sub%c\tsp, #%0-6W"},
  
  , decoder [ARM_EXT_V4T]  0x4700 0xFF80 (BX   <$> reg4 3)-- "bx%c\t%S%x"},
  , decoder [ARM_EXT_V4T]  0x4400 0xFF00 (ADD  <$> reg 0 <*> reg 0 <*> (Reg <$> reg4 3)) -- "add%c\t%D, %S"},
  , decoder [ARM_EXT_V4T]  0x4500 0xFF00 (CMP  <$> reg 0 <*> (Reg <$> reg4 3)) -- "cmp%c\t%D, %S"},
  , decoder [ARM_EXT_V4T]  0x4600 0xFF00 (MOV  <$> reg 0 <*> (Reg <$> reg4 3)) -- "mov%c\t%D, %S"},
  
  , decoder [ARM_EXT_V4T]  0xB400 0xFE00 (PUSH <$> pushRegs) -- "push%c\t%N"},
  , decoder [ARM_EXT_V4T]  0xBC00 0xFE00 (POP  <$> popRegs) -- "pop%c\t%O"},
  
  , decoder [ARM_EXT_V4T]  0x1800 0xFE00 (ADDS <$> reg 0 <*> reg 3 <*> (Reg <$> reg 6)) -- "add%C\t%0-2r, %3-5r, %6-8r"},
  , decoder [ARM_EXT_V4T]  0x1A00 0xFE00 (SUBS <$> reg 0 <*> reg 3 <*> (Reg <$> reg 6)) -- "sub%C\t%0-2r, %3-5r, %6-8r"},
  , decoder [ARM_EXT_V4T]  0x1C00 0xFE00 (ADDS <$> reg 0 <*> reg 3 <*> (Imm <$> integral 6 8)) -- "add%C\t%0-2r, %3-5r, #%6-8d"},
  , decoder [ARM_EXT_V4T]  0x1E00 0xFE00 (SUBS <$> reg 0 <*> reg 3 <*> (Imm <$> integral 6 8)) -- "sub%C\t%0-2r, %3-5r, #%6-8d"},
  
  , decoder [ARM_EXT_V4T]  0x5200 0xFE00 (STRH <$> reg 0 <*> (MemReg <$> reg 3 <*> (Reg <$> reg 6) <*> pure False)) -- "strh%c\t%0-2r, [%3-5r, %6-8r]"},
  , decoder [ARM_EXT_V4T]  0x5A00 0xFE00 (LDRH <$> reg 0 <*> (MemReg <$> reg 3 <*> (Reg <$> reg 6) <*> pure False)) -- "ldrh%c\t%0-2r, [%3-5r, %6-8r]"},
  , decoder [ARM_EXT_V4T]  0x5600 0xF600 (ldr  <$> choose 11 Byte Halfword <*> pure False <*> pure True <*> reg 0 <*> (MemReg <$> reg 3 <*> (Reg <$> reg 6) <*> pure False)) -- "ldrs%11?hb%c\t%0-2r, [%3-5r, %6-8r]"},
  
  , decoder [ARM_EXT_V4T]  0x5000 0xFA00 (str  <$> choose 10 Word Byte <*> pure False <*> reg 0 <*> (MemReg <$> reg 3 <*> (Reg <$> reg 6) <*> pure False)) -- "str%10'b%c\t%0-2r, [%3-5r, %6-8r]"},
  , decoder [ARM_EXT_V4T]  0x5800 0xFA00 (ldr  <$> choose 10 Word Byte <*> pure False <*> pure False <*> reg 0 <*> (MemReg <$> reg 3 <*> (Reg <$> reg 6) <*> pure False)) -- "ldr%10'b%c\t%0-2r, [%3-5r, %6-8r]"},

  , decoder [ARM_EXT_V4T]  0x0000 0xFFC0 (MOVS <$> reg 0 <*> (Reg <$> reg 3)) -- "mov%C\t%0-2r, %3-5r"},
  , decoder [ARM_EXT_V4T]  0x0000 0xF800 (LSLS <$> reg 0 <*> (RegShiftImm S_LSL <$> integral 6 10 <*> reg 3)) -- "lsl%C\t%0-2r, %3-5r, #%6-10d"},
  , decoder [ARM_EXT_V4T]  0x0800 0xF800 (LSRS <$> reg 0 <*> (RegShiftImm S_LSR <$> shift <*> reg 3)) -- "lsr%C\t%0-2r, %3-5r, %s"},
  , decoder [ARM_EXT_V4T]  0x1000 0xF800 (ASRS <$> reg 0 <*> (RegShiftImm S_ASR <$> shift <*> reg 3)) -- "asr%C\t%0-2r, %3-5r, %s"},

  , decoder [ARM_EXT_V4T]  0x2000 0xF800 (MOVS <$> reg 8 <*> (Imm <$> integral 0 7)) -- "mov%C\t%8-10r, #%0-7d"},
  , decoder [ARM_EXT_V4T]  0x2800 0xF800 (CMP  <$> reg 8 <*> (Imm <$> integral 0 7)) -- "cmp%c\t%8-10r, #%0-7d"},
  , decoder [ARM_EXT_V4T]  0x3000 0xF800 (ADDS <$> reg 8 <*> reg 8 <*> (Imm <$> integral 0 7)) -- "add%C\t%8-10r, #%0-7d"},
  , decoder [ARM_EXT_V4T]  0x3800 0xF800 (SUBS <$> reg 8 <*> reg 8 <*> (Imm <$> integral 0 7)) -- "sub%C\t%8-10r, #%0-7d"},

  , decoder [ARM_EXT_V4T]  0x4800 0xF800 (LDR  <$> reg 8 <*> (MemReg PC <$> (Imm <$> ((`shiftL` 2) . integral 0 7)) <*> pure False)) -- "ldr%c\t%8-10r, [pc, #%0-7W]\t; (%0-7a)"},  /* TODO: Disassemble PC relative "LDR rD,=<symbolic>" */

  , decoder [ARM_EXT_V4T]  0x6000 0xF800 (STR  <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> ((`shiftL` 2) . integral 6 10)) <*> pure False)) -- "str%c\t%0-2r, [%3-5r, #%6-10W]"},
  , decoder [ARM_EXT_V4T]  0x6800 0xF800 (LDR  <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> ((`shiftL` 2) . integral 6 10)) <*> pure False)) -- "ldr%c\t%0-2r, [%3-5r, #%6-10W]"},
  , decoder [ARM_EXT_V4T]  0x7000 0xF800 (STRB <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> integral 6 10) <*> pure False)) -- "strb%c\t%0-2r, [%3-5r, #%6-10d]"},
  , decoder [ARM_EXT_V4T]  0x7800 0xF800 (LDRB <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> integral 6 10) <*> pure False)) -- "ldrb%c\t%0-2r, [%3-5r, #%6-10d]"},

  , decoder [ARM_EXT_V4T]  0x8000 0xF800 (STRH <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> ((`shiftL` 1) . integral 6 10)) <*> pure False)) -- "strh%c\t%0-2r, [%3-5r, #%6-10H]"},
  , decoder [ARM_EXT_V4T]  0x8800 0xF800 (LDRH <$> reg 0 <*> (MemReg <$> reg 3 <*> (Imm <$> ((`shiftL` 1) . integral 6 10)) <*> pure False)) -- "ldrh%c\t%0-2r, [%3-5r, #%6-10H]"},

  , decoder [ARM_EXT_V4T]  0x9000 0xF800 (STR  <$> reg 8 <*> (MemReg SP <$> (Imm <$> ((`shiftL` 2) . integral 0 7)) <*> pure False)) -- "str%c\t%8-10r, [sp, #%0-7W]"},
  , decoder [ARM_EXT_V4T]  0x9800 0xF800 (LDR  <$> reg 8 <*> (MemReg SP <$> (Imm <$> ((`shiftL` 2) . integral 0 7)) <*> pure False)) -- "ldr%c\t%8-10r, [sp, #%0-7W]"},

  , decoder [ARM_EXT_V4T]  0xA000 0xF800 (ADD  <$> reg 8 <*> pure PC <*> (Imm <$> ((`shiftL` 2) . integral 0 7))) -- "add%c\t%8-10r, pc, #%0-7W\t; (adr %8-10r, %0-7a)"},
  , decoder [ARM_EXT_V4T]  0xA800 0xF800 (ADD  <$> reg 8 <*> pure SP <*> (Imm <$> ((`shiftL` 2) . integral 0 7))) -- "add%c\t%8-10r, sp, #%0-7W"},

  , decoder [ARM_EXT_V4T]  0xC000 0xF800 (STM  True <$> reg 8 <*> (Regs <$> select registers <$> bits 0 7)) -- "stmia%c\t%8-10r!, %M"},
  , decoder [ARM_EXT_V4T]  0xC800 0xF800 (LDM  <$> (notElem <$> reg 8 <*> (select registers <$> bits 0 7)) <*> reg 8 <*> (Regs <$> select registers <$> bits 0 7)) -- FIXME: fugly -- "ldmia%c\t%8-10r%W, %M"},
                                               
  , decoder [ARM_EXT_V4T]  0xDF00 0xFF00 (SVC  <$> integral 0 7) -- "svc%c\t%0-7d"}, 

  , decoder [ARM_EXT_V4T]  0xDE00 0xFE00 (pure Undefined)
  , decoder [ARM_EXT_V4T]  0xD000 0xF000 (Conditional <$> condition <*> (B <$> ((`shiftL` 1) . integral 0 7))) -- FIXME: sign-extend -- "b%8-11c.n\t%0-7B%X"},  
  
  , decoder [ARM_EXT_V4T]  0xE000 0xF800 (B <$> ((`shiftL` 1) . integral 0 10)) -- FIXME: sign-extend -- "b%c.n\t%0-10B%x"},
  
  , decoder [ARM_EXT_V1]   0x0000 0x000 (pure Undefined)
  ]


thumbDecode :: Word16 -> GeneralInstruction UAL
thumbDecode i = fromMaybe Undefined . fmap (decode Thumb i) . find (decoderMatches Thumb i) $ thumbDecoders

thumbDecodeDbg :: Word16 -> (GeneralInstruction UAL, (Word16, Word16))
thumbDecodeDbg i = case fromJust . find (decoderMatches Thumb i) $ thumbDecoders of
                    GeneralDecoder a b c d -> (d i, (b, c))