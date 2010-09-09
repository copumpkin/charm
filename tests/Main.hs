module Main where

import Data.Word

import Architecture.ARM.Decoder.ARM
import Architecture.ARM.Pretty

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Objdump
import System.Random

import Data.Char
import Data.Maybe

main = 
  do rs <- fmap (take 100 . randoms) getStdGen
     let ws = map fromIntegral (rs :: [Int]) :: [Word32]
     insns <- disassemble "/Users/pumpkin/arm-none-eabi/bin/arm-none-eabi-objdump" ["-b", "binary", "-m", "arm"] ws
     let uppers = map (map toUpper) insns
     let ours = map (showInstruction . fromJust . armDecode) ws
     let tests = zipWith (\o u -> testCase u (o @?= u)) ours uppers
     defaultMain [testGroup "Instructions from objdump" tests]