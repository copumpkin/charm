module Architecture.ARM.Decoder.NEON where

import Data.Word hiding (Word)

import Architecture.ARM.Common

data NEON = NEON
type instance Word NEON = Word32

