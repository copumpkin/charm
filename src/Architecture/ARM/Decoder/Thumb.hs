{-# Language MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Architecture.ARM.Decoder.Thumb where

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

data Thumb = Thumb
type instance Word Thumb = Word16

instance Decoder Thumb (Instruction UAL Conditional) where
  type DecoderType Thumb (Instruction UAL Conditional) = GeneralDecoder (Word Thumb) (GeneralInstruction UAL)

  decoder e s v m d = undefined

instance Decoder Thumb (Instruction UAL Unconditional) where
  type DecoderType Thumb (Instruction UAL Unconditional) = GeneralDecoder (Word Thumb) (GeneralInstruction UAL)
  
  decoder e s v m d = GeneralDecoder s v m (Unconditional <$> d)

instance Decoder a (GeneralInstruction i) where
  type DecoderType a (GeneralInstruction i) = GeneralDecoder (Word a) (GeneralInstruction i)
  
  decoder e s v m d = GeneralDecoder s v m d




type D a = Word16 -> a



thumb_c = undefined


thumbDecoders = 
  [ decoder Thumb [ARM_EXT_V6K] 0xbf00 0xffff (pure NOP)
  , decoder Thumb [ARM_EXT_V6K] 0xbf10 0xffff (pure YIELD)
  , decoder Thumb [ARM_EXT_V6K] 0xbf20 0xffff (pure WFE)
  , decoder Thumb [ARM_EXT_V6K] 0xbf30 0xffff (pure WFI)
  , decoder Thumb [ARM_EXT_V6K] 0xbf40 0xffff (pure SEV)
  ]