module Architecture.ARM.Decoder where

import Data.Word
import Data.Int
-- import Data.IntMap
import Data.Maybe

import Architecture.ARM.Instructions.UAL
import Architecture.ARM.Decoder.ARM
import Architecture.ARM.Decoder.Thumb

{-
data BasicBlock = BasicBlock { instructions :: [Instruction] 
                             , successors   :: [Word32]
                             }
  deriving (Show, Eq)

decode :: (Word32 -> Word8) -> Word32 -> Maybe BasicBlock
decode f x = undefined
           
main = do instructions <- fmap (head . read) $ readFile "../tests/test.raw"
          let f i = fromJust $ lookup i instructions
          print $ decode f 0x2000
-}