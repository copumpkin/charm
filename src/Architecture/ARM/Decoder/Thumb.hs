{-# Language MultiParamTypeClasses, TypeFamilies #-}
module Architecture.ARM.Decoder.Thumb where

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

type D a = Word16 -> a

instance Decoder Word16 Conditional where
  type Structure Word16 Conditional = ARMDecoder Word16 UALInstruction
  
  decoder archs value mask d = decoder archs value mask (Conditional <$> thumb_c <*> d)
  
instance Decoder Word16 Unconditional where
  type Structure Word16 Unconditional = ARMDecoder Word16 UALInstruction

  decoder archs value mask d = decoder archs value mask (Unconditional <$> d)

instance Decoder Word16 UALInstruction where
  type Structure Word16 UALInstruction = ARMDecoder Word16 UALInstruction
  
  decoder = ARMDecoder

thumb_c = undefined

pure16 :: a -> D a
pure16 = pure

thumbOpcodes :: [ARMDecoder Word16 UALInstruction]
thumbOpcodes = 
  [ decoder [ARM_EXT_V6K] 0xbf00 0xffff (pure16 NOP)
  , decoder [ARM_EXT_V6K] 0xbf10 0xffff (pure16 YIELD)
  , decoder [ARM_EXT_V6K] 0xbf20 0xffff (pure16 WFE)
  , decoder [ARM_EXT_V6K] 0xbf30 0xffff (pure16 WFI)
  , decoder [ARM_EXT_V6K] 0xbf40 0xffff (pure16 SEV)
  ]