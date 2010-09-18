module Architecture.ARM.Decoder.Common where

import Data.Int
import Data.Word
import Data.Bits
import Data.List

import Architecture.ARM.Common

import Control.Applicative
import Control.Monad

range :: Int -> Int -> Int
range s e = e - s + 1

ones :: Bits a => Int -> a
ones n = bit n - 1

mask :: Bits a => Int -> Int -> a
mask s e = ones (range s e) `shiftL` s

bitRange :: Bits a => Int -> Int -> a -> a
bitRange s e i = (i `shiftR` s) .&. ones (range s e)

bitRanges :: (Bits a, Integral a) => [(Int, Int)] -> a -> a
bitRanges xs i = foldl' (.|.) 0 shiftedValues
  where shifts = scanl ((. uncurry range) . (+)) 0 xs
        values = mapM (uncurry bitRange) xs i
        shiftedValues = zipWith shiftL values shifts

allSet :: Bits a => Int -> Int -> a -> Bool
allSet s e = (== ones (range s e)) . bitRange s e

noneSet :: Bits a => Int -> Int -> a -> Bool
noneSet s e = (== 0) . bitRange s e

integral :: (Bits a, Integral a, Integral b) => Int -> Int -> a -> b
integral s e = fromIntegral . bitRange s e

integral2 :: (Bits a, Integral a, Integral b) => Int -> Int -> a -> b
integral2 s e = (2*) . integral s e

integral4 :: (Bits a, Integral a, Integral b) => Int -> Int -> a -> b
integral4 s e = (4*) . integral s e

multiIntegral :: (Bits a, Integral a, Integral b) => [(Int, Int)] -> a -> b
multiIntegral = (fromIntegral .) . bitRanges


bool :: Bits a => Int -> a -> Bool
bool = flip testBit

choose :: Bits a => Int -> b -> b -> a -> b
choose b t f i = if bool b i then t else f

bits :: Bits a => Int -> Int -> a -> [Bool]
bits = (mapM bool .) . enumFromTo

direction :: Bits a => Int -> a -> Direction
direction n = choose n Increment Decrement

order :: Bits a => Int -> a -> Order
order n = choose n Before After

nybble :: Bits a => Int -> a -> Nybble
nybble n = choose n High Low


reg3 :: (Bits a, Integral a) => Int -> a -> Register
reg3 s = toEnum . integral s (s + 2)

reg4 :: (Bits a, Integral a) => Int -> a -> Register
reg4 s = toEnum . integral s (s + 3)

hint :: (Bits a, Integral a) => a -> Hint
hint =
  do opt <- integral 0 3
     return $ case opt of
       2  -> OSHST
       3  -> OSH
       6  -> NSH
       7  -> NSHST
       10 -> ISH
       11 -> ISHST
       14 -> SY
       15 -> ST
       x  -> InvalidHint x