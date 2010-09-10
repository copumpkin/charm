module Main where

import Data.Word

import Architecture.ARM.Instructions.UAL
import Architecture.ARM.Decoder.ARM
import Architecture.ARM.Pretty

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Objdump
import System.Random

import Data.Char
import Data.List
import Data.Maybe

import Text.Printf

ualMatches :: Word32 -> UALInstruction -> String -> Assertion
ualMatches off Undefined s = assertBool ("undefined mismatch with " ++ show s) ("invalid" `isInfixOf` s || "undefined" `isInfixOf` s)
ualMatches off (Conditional cond (B op)) s = map toLower (showInstruction (Conditional cond (B $ op + (fromIntegral off)))) @?= s
ualMatches off (Conditional cond (BL op)) s = map toLower (showInstruction (Conditional cond (BL $ op + (fromIntegral off)))) @?= s
ualMatches off (Unconditional (BLXUC op)) s = map toLower (showInstruction (Unconditional (BLXUC $ op + (fromIntegral off)))) @?= s
ualMatches off x s = map toLower (showInstruction x) @?= s

main = 
  do rs <- fmap (take 1000 . randoms) getStdGen
     let ws = map fromIntegral (rs :: [Int]) :: [Word32]
     insns <- disassemble "/Users/pumpkin/arm-none-eabi/bin/arm-none-eabi-objdump" ["-b", "binary", "-m", "arm"] ws
     let ours = map armDecode ws
     let tests = zipWith4 (\off w o u -> testCase (printf "0x%08x: 0x%08x %s" off w u) (ualMatches off o u)) [0,4..] ws ours insns
     defaultMain [testGroup "Instructions from objdump" tests]
