{-# Language MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Architecture.ARM.Decoder.Thumb32 (Thumb32, thumb32Decode, thumb32DecodeDbg) where

import Prelude hiding (and)

import Data.Word hiding (Word)
import Data.Maybe
import Data.List hiding (and)

import Architecture.ARM.Common
import Architecture.ARM.Instructions.UAL

import Control.Monad
import Control.Applicative

data Thumb32 = Thumb32
type instance Word Thumb32 = Word32

instance Decoder Thumb32 (Instruction UAL Conditional) where
  type Target Thumb32 (Instruction UAL Conditional) = GeneralInstruction UAL

  decoder s v m d = GeneralDecoder s v m (Conditional AL <$> d) 

instance Decoder Thumb32 (Instruction UAL Unconditional) where
  type Target Thumb32 (Instruction UAL Unconditional) = GeneralInstruction UAL
  
  decoder s v m d = GeneralDecoder s v m (Unconditional <$> d)

instance Decoder Thumb32 (GeneralInstruction a) where
  type Target Thumb32 (GeneralInstruction a) = GeneralInstruction a

  decoder s v m d = GeneralDecoder s v m d

type D a = Word32 -> a

reg :: Int -> D Register
reg = undefined

bool :: Int -> D Bool
bool = undefined

multi :: D MultiRegOp
multi = undefined

thumb32Decoders :: [GeneralDecoder Thumb32 (GeneralInstruction UAL)]
thumb32Decoders =
  [ decoder [ARM_EXT_V7]   0xf910f000 0xff70f000 (PLI <$> undefined) -- "pli%c\t%a"},
  , decoder [ARM_EXT_V7]   0xf3af80f0 0xfffffff0 (DBG <$> undefined) -- "dbg%c\t#%0-3d"},
  , decoder [ARM_EXT_V7]   0xf3bf8f50 0xfffffff0 (DMB <$> undefined) -- "dmb%c\t%U"},
  , decoder [ARM_EXT_V7]   0xf3bf8f40 0xfffffff0 (DSB <$> undefined) -- "dsb%c\t%U"},
  , decoder [ARM_EXT_V7]   0xf3bf8f60 0xfffffff0 (ISB <$> undefined) -- "isb%c\t%U"},
  , decoder [ARM_EXT_DIV]  0xfb90f0f0 0xfff0f0f0 (SDIV <$> reg 8 <*> reg 16 <*> reg 0) -- "sdiv%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_DIV]  0xfbb0f0f0 0xfff0f0f0 (SDIV <$> reg 8 <*> reg 16 <*> reg 0) -- "udiv%c\t%8-11r, %16-19r, %0-3r"},


  , decoder [ARM_EXT_V6T2] 0xf3af8000 0xffffffff (pure NOP) -- "nop%c.w"},
  , decoder [ARM_EXT_V6T2] 0xf3af8001 0xffffffff (pure YIELD) -- "yield%c.w"},
  , decoder [ARM_EXT_V6T2] 0xf3af8002 0xffffffff (pure WFE) -- "wfe%c.w"},
  , decoder [ARM_EXT_V6T2] 0xf3af8003 0xffffffff (pure WFI) -- "wfi%c.w"},
  , decoder [ARM_EXT_V6T2] 0xf3af8004 0xffffffff (pure SEV) -- "sev%c.w"},
  , decoder [ARM_EXT_V6T2] 0xf3af8000 0xffffff00 (pure NOP) -- "nop%c.w\t{%0-7d}"},

  , decoder [ARM_EXT_V6T2] 0xf3bf8f2f 0xffffffff (pure CLREX) -- "clrex%c"},
  , decoder [ARM_EXT_V6T2] 0xf3af8400 0xffffff1f (CPSIE <$> bool 7 <*> bool 6 <*> bool 5 <*> undefined) -- "cpsie.w\t%7'a%6'i%5'f%X"},
  , decoder [ARM_EXT_V6T2] 0xf3af8600 0xffffff1f (CPSID <$> bool 7 <*> bool 6 <*> bool 5 <*> undefined) -- "cpsid.w\t%7'a%6'i%5'f%X"},
  , decoder [ARM_EXT_V6T2] 0xf3c08f00 0xfff0ffff (BXJ   <$> reg 16) -- "bxj%c\t%16-19r%x"},
  , decoder [ARM_EXT_V6T2] 0xe810c000 0xffd0ffff (RFEDB <$> bool 21 <*> reg 16) -- "rfedb%c\t%16-19r%21'!"},
  , decoder [ARM_EXT_V6T2] 0xe990c000 0xffd0ffff (RFE   <$> bool 21 <*> reg 16) -- "rfeia%c\t%16-19r%21'!"},
  , decoder [ARM_EXT_V6T2] 0xf3ef8000 0xffeff000 (MRS   <$> reg 8 <*> undefined) -- "mrs%c\t%8-11r, %D"},
  , decoder [ARM_EXT_V6T2] 0xf3af8100 0xffffffe0 (CPS   <$> undefined) -- "cps\t#%0-4d%X"},
  , decoder [ARM_EXT_V6T2] 0xe8d0f000 0xfff0fff0 (TBB   <$> undefined) -- "tbb%c\t[%16-19r, %0-3r]%x"},
  , decoder [ARM_EXT_V6T2] 0xe8d0f010 0xfff0fff0 (TBH   <$> undefined) -- "tbh%c\t[%16-19r, %0-3r, lsl #1]%x"},
  , decoder [ARM_EXT_V6T2] 0xf3af8500 0xffffff00 (CPSIE <$> bool 7 <*> bool 6 <*> bool 5 <*> undefined) -- "cpsie\t%7'a%6'i%5'f, #%0-4d%X"},
  , decoder [ARM_EXT_V6T2] 0xf3af8700 0xffffff00 (CPSID <$> bool 7 <*> bool 6 <*> bool 5 <*> undefined) -- "cpsid\t%7'a%6'i%5'f, #%0-4d%X"},
  , decoder [ARM_EXT_V6T2] 0xf3de8f00 0xffffff00 (SUBS PC LR <$> undefined) -- "subs%c\tpc, lr, #%0-7d"},
  , decoder [ARM_EXT_V6T2] 0xf3808000 0xffe0f000 (MSR   <$> undefined <*> undefined <*> (Reg <$> reg 16)) -- "msr%c\t%C, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xe8500f00 0xfff00fff (LDREX <$> reg 12 <*> (MemReg <$> reg 16 <*> pure (Imm 0) <*> pure False)) -- "ldrex%c\t%12-15r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xe8d00f4f 0xfff00fef (pure Undefined) -- "ldrex%4?hb%c\t%12-15r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xe800c000 0xffd0ffe0 (SRSDB <$> bool 21 <*> reg 16 <*> undefined) -- "srsdb%c\t%16-19r%21'!, #%0-4d"},
  , decoder [ARM_EXT_V6T2] 0xe980c000 0xffd0ffe0 (SRS   <$> bool 21 <*> reg 16 <*> undefined) -- "srsia%c\t%16-19r%21'!, #%0-4d"},
  , decoder [ARM_EXT_V6T2] 0xfa0ff080 0xfffff0c0 (pure Undefined) -- "sxth%c.w\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa1ff080 0xfffff0c0 (pure Undefined) -- "uxth%c.w\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa2ff080 0xfffff0c0 (pure Undefined) -- "sxtb16%c\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa3ff080 0xfffff0c0 (pure Undefined) -- "uxtb16%c\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa4ff080 0xfffff0c0 (pure Undefined) -- "sxtb%c.w\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa5ff080 0xfffff0c0 (pure Undefined) -- "uxtb%c.w\t%8-11r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xe8400000 0xfff000ff (pure Undefined) -- "strex%c\t%8-11r, %12-15r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xe8d0007f 0xfff000ff (pure Undefined) -- "ldrexd%c\t%12-15r, %8-11r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xfa80f000 0xfff0f0f0 (pure Undefined) -- "sadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f010 0xfff0f0f0 (pure Undefined) -- "qadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f020 0xfff0f0f0 (pure Undefined) -- "shadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f040 0xfff0f0f0 (pure Undefined) -- "uadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f050 0xfff0f0f0 (pure Undefined) -- "uqadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f060 0xfff0f0f0 (pure Undefined) -- "uhadd8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f080 0xfff0f0f0 (pure Undefined) -- "qadd%c\t%8-11r, %0-3r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f090 0xfff0f0f0 (pure Undefined) -- "qdadd%c\t%8-11r, %0-3r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f0a0 0xfff0f0f0 (pure Undefined) -- "qsub%c\t%8-11r, %0-3r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa80f0b0 0xfff0f0f0 (pure Undefined) -- "qdsub%c\t%8-11r, %0-3r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f000 0xfff0f0f0 (pure Undefined) -- "sadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f010 0xfff0f0f0 (pure Undefined) -- "qadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f020 0xfff0f0f0 (pure Undefined) -- "shadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f040 0xfff0f0f0 (pure Undefined) -- "uadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f050 0xfff0f0f0 (pure Undefined) -- "uqadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f060 0xfff0f0f0 (pure Undefined) -- "uhadd16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f080 0xfff0f0f0 (pure Undefined) -- "rev%c.w\t%8-11r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f090 0xfff0f0f0 (pure Undefined) -- "rev16%c.w\t%8-11r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f0a0 0xfff0f0f0 (pure Undefined) -- "rbit%c\t%8-11r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfa90f0b0 0xfff0f0f0 (pure Undefined) -- "revsh%c.w\t%8-11r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f000 0xfff0f0f0 (pure Undefined) -- "sasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f010 0xfff0f0f0 (pure Undefined) -- "qasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f020 0xfff0f0f0 (pure Undefined) -- "shasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f040 0xfff0f0f0 (pure Undefined) -- "uasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f050 0xfff0f0f0 (pure Undefined) -- "uqasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f060 0xfff0f0f0 (pure Undefined) -- "uhasx%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfaa0f080 0xfff0f0f0 (pure Undefined) -- "sel%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfab0f080 0xfff0f0f0 (pure Undefined) -- "clz%c\t%8-11r, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f000 0xfff0f0f0 (pure Undefined) -- "ssub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f010 0xfff0f0f0 (pure Undefined) -- "qsub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f020 0xfff0f0f0 (pure Undefined) -- "shsub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f040 0xfff0f0f0 (pure Undefined) -- "usub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f050 0xfff0f0f0 (pure Undefined) -- "uqsub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfac0f060 0xfff0f0f0 (pure Undefined) -- "uhsub8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f000 0xfff0f0f0 (pure Undefined) -- "ssub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f010 0xfff0f0f0 (pure Undefined) -- "qsub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f020 0xfff0f0f0 (pure Undefined) -- "shsub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f040 0xfff0f0f0 (pure Undefined) -- "usub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f050 0xfff0f0f0 (pure Undefined) -- "uqsub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfad0f060 0xfff0f0f0 (pure Undefined) -- "uhsub16%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f000 0xfff0f0f0 (pure Undefined) -- "ssax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f010 0xfff0f0f0 (pure Undefined) -- "qsax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f020 0xfff0f0f0 (pure Undefined) -- "shsax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f040 0xfff0f0f0 (pure Undefined) -- "usax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f050 0xfff0f0f0 (pure Undefined) -- "uqsax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfae0f060 0xfff0f0f0 (pure Undefined) -- "uhsax%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfb00f000 0xfff0f0f0 (pure Undefined) -- "mul%c.w\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfb70f000 0xfff0f0f0 (pure Undefined) -- "usad8%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa00f000 0xffe0f0f0 (pure Undefined) -- "lsl%20's%c.w\t%8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfa20f000 0xffe0f0f0 (pure Undefined) -- "lsr%20's%c.w\t%8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfa40f000 0xffe0f0f0 (pure Undefined) -- "asr%20's%c.w\t%8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfa60f000 0xffe0f0f0 (pure Undefined) -- "ror%20's%c.w\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xe8c00f40 0xfff00fe0 (pure Undefined) -- "strex%4?hb%c\t%0-3r, %12-15r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xf3200000 0xfff0f0e0 (pure Undefined) -- "ssat16%c\t%8-11r, #%0-4d, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xf3a00000 0xfff0f0e0 (pure Undefined) -- "usat16%c\t%8-11r, #%0-4d, %16-19r"},
  , decoder [ARM_EXT_V6T2] 0xfb20f000 0xfff0f0e0 (pure Undefined) -- "smuad%4'x%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfb30f000 0xfff0f0e0 (pure Undefined) -- "smulw%4?tb%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfb40f000 0xfff0f0e0 (pure Undefined) -- "smusd%4'x%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfb50f000 0xfff0f0e0 (pure Undefined) -- "smmul%4'r%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xfa00f080 0xfff0f0c0 (pure Undefined) -- "sxtah%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa10f080 0xfff0f0c0 (pure Undefined) -- "uxtah%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa20f080 0xfff0f0c0 (pure Undefined) -- "sxtab16%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa30f080 0xfff0f0c0 (pure Undefined) -- "uxtab16%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa40f080 0xfff0f0c0 (pure Undefined) -- "sxtab%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfa50f080 0xfff0f0c0 (pure Undefined) -- "uxtab%c\t%8-11r, %16-19r, %0-3r%R"},
  , decoder [ARM_EXT_V6T2] 0xfb10f000 0xfff0f0c0 (pure Undefined) -- "smul%5?tb%4?tb%c\t%8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xf36f0000 0xffff8020 (pure Undefined) -- "bfc%c\t%8-11r, %E"},
  , decoder [ARM_EXT_V6T2] 0xea100f00 0xfff08f00 (pure Undefined) -- "tst%c.w\t%16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea900f00 0xfff08f00 (pure Undefined) -- "teq%c\t%16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeb100f00 0xfff08f00 (pure Undefined) -- "cmn%c.w\t%16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xebb00f00 0xfff08f00 (pure Undefined) -- "cmp%c.w\t%16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xf0100f00 0xfbf08f00 (pure Undefined) -- "tst%c.w\t%16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf0900f00 0xfbf08f00 (pure Undefined) -- "teq%c\t%16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1100f00 0xfbf08f00 (pure Undefined) -- "cmn%c.w\t%16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1b00f00 0xfbf08f00 (pure Undefined) -- "cmp%c.w\t%16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xea4f0000 0xffef8000 (pure Undefined) -- "mov%20's%c.w\t%8-11r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea6f0000 0xffef8000 (pure Undefined) -- "mvn%20's%c.w\t%8-11r, %S"},
  , decoder [ARM_EXT_V6T2] 0xe8c00070 0xfff000f0 (pure Undefined) -- "strexd%c\t%0-3r, %12-15r, %8-11r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xfb000000 0xfff000f0 (pure Undefined) -- "mla%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
  , decoder [ARM_EXT_V6T2] 0xfb000010 0xfff000f0 (pure Undefined) -- "mls%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
  , decoder [ARM_EXT_V6T2] 0xfb700000 0xfff000f0 (pure Undefined) -- "usada8%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfb800000 0xfff000f0 (SMULL <$> reg 12 <*> reg 8 <*> reg 16 <*> reg 0) -- "smull%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfba00000 0xfff000f0 (UMULL <$> reg 12 <*> reg 8 <*> reg 16 <*> reg 0) -- "umull%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfbc00000 0xfff000f0 (SMLAL <$> reg 12 <*> reg 8 <*> reg 16 <*> reg 0) -- "smlal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfbe00000 0xfff000f0 (UMLAL <$> reg 12 <*> reg 8 <*> reg 16 <*> reg 0) -- "umlal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfbe00060 0xfff000f0 (UMAAL <$> reg 12 <*> reg 8 <*> reg 16 <*> reg 0) -- "umaal%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xe8500f00 0xfff00f00 (pure Undefined) -- "ldrex%c\t%12-15r, [%16-19r, #%0-7W]"},
  , decoder [ARM_EXT_V6T2] 0xf7f08000 0xfff0f000 (pure Undefined) -- "smc%c\t%K"},
  , decoder [ARM_EXT_V6T2] 0xf04f0000 0xfbef8000 (pure Undefined) -- "mov%20's%c.w\t%8-11r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf06f0000 0xfbef8000 (pure Undefined) -- "mvn%20's%c.w\t%8-11r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf810f000 0xff70f000 (pure Undefined) -- "pld%c\t%a"},
  , decoder [ARM_EXT_V6T2] 0xfb200000 0xfff000e0 (smlad <$> bool 4 <*> reg 8 <*> reg 16 <*> reg 0 <*> reg 12) -- "smlad%4'x%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfb300000 0xfff000e0 (pure Undefined) -- <*> reg 8 <*> reg 16 <*> reg 0 <*> reg 12) -- "smlaw%4?tb%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfb400000 0xfff000e0 (smlsd <$> bool 4 <*> reg 8 <*> reg 16 <*> reg 0 <*> reg 12) -- "smlsd%4'x%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfb500000 0xfff000e0 (smmla <$> bool 4 <*> reg 8 <*> reg 16 <*> reg 0 <*> reg 12) -- "smmla%4'r%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfb600000 0xfff000e0 (smmls <$> bool 4 <*> reg 8 <*> reg 16 <*> reg 0 <*> reg 12) -- "smmls%4'r%c\t%8-11R, %16-19R, %0-3R, %12-15R"},
  , decoder [ARM_EXT_V6T2] 0xfbc000c0 0xfff000e0 (pure Undefined) -- "smlald%4'x%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xfbd000c0 0xfff000e0 (pure Undefined) -- "smlsld%4'x%c\t%12-15R, %8-11R, %16-19R, %0-3R"},
  , decoder [ARM_EXT_V6T2] 0xeac00000 0xfff08030 (pure Undefined) -- "pkhbt%c\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeac00020 0xfff08030 (pure Undefined) -- "pkhtb%c\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xf3400000 0xfff08020 (pure Undefined) -- "sbfx%c\t%8-11r, %16-19r, %F"},
  , decoder [ARM_EXT_V6T2] 0xf3c00000 0xfff08020 (pure Undefined) -- "ubfx%c\t%8-11r, %16-19r, %F"},
  , decoder [ARM_EXT_V6T2] 0xf8000e00 0xff900f00 (pure Undefined) -- "str%wt%c\t%12-15r, %a"},
  , decoder [ARM_EXT_V6T2] 0xfb100000 0xfff000c0 (pure Undefined) -- "smla%5?tb%4?tb%c\t%8-11r, %16-19r, %0-3r, %12-15r"},
  , decoder [ARM_EXT_V6T2] 0xfbc00080 0xfff000c0 (pure Undefined) -- "smlal%5?tb%4?tb%c\t%12-15r, %8-11r, %16-19r, %0-3r"},
  , decoder [ARM_EXT_V6T2] 0xf3600000 0xfff08020 (pure Undefined) -- "bfi%c\t%8-11r, %16-19r, %E"},
  , decoder [ARM_EXT_V6T2] 0xf8100e00 0xfe900f00 (pure Undefined) -- "ldr%wt%c\t%12-15r, %a"},
  , decoder [ARM_EXT_V6T2] 0xf3000000 0xffd08020 (SSAT <$> reg 8 <*> undefined <*> (Reg <$> reg 16)) -- "ssat%c\t%8-11r, #%0-4d, %16-19r%s"},
  , decoder [ARM_EXT_V6T2] 0xf3800000 0xffd08020 (USAT <$> reg 8 <*> undefined <*> (Reg <$> reg 16)) -- "usat%c\t%8-11r, #%0-4d, %16-19r%s"},
  , decoder [ARM_EXT_V6T2] 0xf2000000 0xfbf08000 (pure Undefined) -- "addw%c\t%8-11r, %16-19r, %I"},
  , decoder [ARM_EXT_V6T2] 0xf2400000 0xfbf08000 (pure Undefined) -- "movw%c\t%8-11r, %J"},
  , decoder [ARM_EXT_V6T2] 0xf2a00000 0xfbf08000 (pure Undefined) -- "subw%c\t%8-11r, %16-19r, %I"},
  , decoder [ARM_EXT_V6T2] 0xf2c00000 0xfbf08000 (pure Undefined) -- "movt%c\t%8-11r, %J"},
  , decoder [ARM_EXT_V6T2] 0xea000000 0xffe08000 (and   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "and%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea200000 0xffe08000 (bic   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "bic%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea400000 0xffe08000 (orr   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "orr%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea600000 0xffe08000 (orn   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "orn%20's%c\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xea800000 0xffe08000 (eor   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "eor%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeb000000 0xffe08000 (add   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "add%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeb400000 0xffe08000 (adc   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "adc%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeb600000 0xffe08000 (sbc   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "sbc%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xeba00000 0xffe08000 (sub   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "sub%20's%c.w\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xebc00000 0xffe08000 (rsb   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "rsb%20's%c\t%8-11r, %16-19r, %S"},
  , decoder [ARM_EXT_V6T2] 0xe8400000 0xfff00000 (pure Undefined) -- "strex%c\t%8-11r, %12-15r, [%16-19r, #%0-7W]"},
  , decoder [ARM_EXT_V6T2] 0xf0000000 0xfbe08000 (and   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "and%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf0200000 0xfbe08000 (bic   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "bic%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf0400000 0xfbe08000 (orr   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "orr%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf0600000 0xfbe08000 (orn   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "orn%20's%c\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf0800000 0xfbe08000 (eor   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "eor%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1000000 0xfbe08000 (add   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "add%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1400000 0xfbe08000 (adc   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "adc%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1600000 0xfbe08000 (sbc   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "sbc%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1a00000 0xfbe08000 (sub   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "sub%20's%c.w\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xf1c00000 0xfbe08000 (rsb   <$> bool 20 <*> reg 8 <*> reg 16 <*> undefined) -- "rsb%20's%c\t%8-11r, %16-19r, %M"},
  , decoder [ARM_EXT_V6T2] 0xe8800000 0xffd00000 (STM   <$> bool 21 <*> reg 16 <*> multi) -- "stmia%c.w\t%16-19r%21'!, %m"},
  , decoder [ARM_EXT_V6T2] 0xe8900000 0xffd00000 (LDM   <$> bool 21 <*> reg 16 <*> multi) -- "ldmia%c.w\t%16-19r%21'!, %m"},
  , decoder [ARM_EXT_V6T2] 0xe9000000 0xffd00000 (STMDB <$> bool 21 <*> reg 16 <*> multi) -- "stmdb%c\t%16-19r%21'!, %m"},
  , decoder [ARM_EXT_V6T2] 0xe9100000 0xffd00000 (LDMDB <$> bool 21 <*> reg 16 <*> multi) -- "ldmdb%c\t%16-19r%21'!, %m"},
  , decoder [ARM_EXT_V6T2] 0xe9c00000 0xffd000ff (STRD  <$> reg 12 <*> reg 8 <*> undefined) -- "strd%c\t%12-15r, %8-11r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xe9d00000 0xffd000ff (LDRD  <$> reg 12 <*> reg 8 <*> undefined) -- "ldrd%c\t%12-15r, %8-11r, [%16-19r]"},
  , decoder [ARM_EXT_V6T2] 0xe9400000 0xff500000 (STRD  <$> reg 12 <*> reg 8 <*> undefined) -- "strd%c\t%12-15r, %8-11r, [%16-19r, #%23`-%0-7W]%21'!"},
  , decoder [ARM_EXT_V6T2] 0xe9500000 0xff500000 (LDRD  <$> reg 12 <*> reg 8 <*> undefined) -- "ldrd%c\t%12-15r, %8-11r, [%16-19r, #%23`-%0-7W]%21'!"},
  , decoder [ARM_EXT_V6T2] 0xe8600000 0xff700000 (STRD  <$> reg 12 <*> reg 8 <*> undefined) -- "strd%c\t%12-15r, %8-11r, [%16-19r], #%23`-%0-7W"},
  , decoder [ARM_EXT_V6T2] 0xe8700000 0xff700000 (LDRD  <$> reg 12 <*> reg 8 <*> undefined) -- "ldrd%c\t%12-15r, %8-11r, [%16-19r], #%23`-%0-7W"},
  , decoder [ARM_EXT_V6T2] 0xf8000000 0xff100000 (pure Undefined) -- "str%w%c.w\t%12-15r, %a"},
  , decoder [ARM_EXT_V6T2] 0xf8100000 0xfe100000 (pure Undefined) -- "ldr%w%c.w\t%12-15r, %a"},

  , decoder [ARM_EXT_V6T2] 0xf3c08000 0xfbc0d000 (pure Undefined) -- "(pure Undefined) (bcc, cond=0xF)"},
  , decoder [ARM_EXT_V6T2] 0xf3808000 0xfbc0d000 (pure Undefined) -- "(pure Undefined) (bcc, cond=0xE)"},
  , decoder [ARM_EXT_V6T2] 0xf0008000 0xf800d000 (pure Undefined) -- "b%22-25c.w\t%b%X"},
  , decoder [ARM_EXT_V6T2] 0xf0009000 0xf800d000 (pure Undefined) -- "b%c.w\t%B%x"},


  , decoder [ARM_EXT_V4T]  0xf000c000 0xf800d000 (pure Undefined) -- "blx%c\t%B%x"},
  , decoder [ARM_EXT_V4T]  0xf000d000 0xf800d000 (pure Undefined) -- "bl%c\t%B%x"},

  , decoder [ARM_EXT_V1]   0x00000000 0x00000000 (pure Undefined)
  ]

-- FIXME: might need to do some bit-twiddling
thumb32Decode :: Word32 -> GeneralInstruction UAL
thumb32Decode i = fromMaybe Undefined . fmap (decode Thumb32 i) . find (decoderMatches Thumb32 i) $ thumb32Decoders

thumb32DecodeDbg :: Word32 -> (GeneralInstruction UAL, (Word32, Word32))
thumb32DecodeDbg i = case fromJust . find (decoderMatches Thumb32 i) $ thumb32Decoders of
                    GeneralDecoder a b c d -> (d i, (b, c))