module Architecture.ARM.Decoder.Thumb2 where

import Data.Word hiding (Word)

import Architecture.ARM.Common

data Thumb2 = Thumb2
type instance Word Thumb2 = Word32

