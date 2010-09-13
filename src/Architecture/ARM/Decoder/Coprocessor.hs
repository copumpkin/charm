module Architecture.ARM.Decoder.Coprocessor where

import Data.Word hiding (Word)

import Architecture.ARM.Common

data Coprocessor = Coprocessor
type instance Word Coprocessor = Word32

