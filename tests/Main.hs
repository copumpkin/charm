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
ualMatches off i@(Conditional _ _) s | "invalid" `isInfixOf` s || "undefined" `isInfixOf` s = assertFailure (printf "invalid instruction '%s' decoded to '%s'" s (showInstruction i))
ualMatches off i@(Unconditional _) s | "invalid" `isInfixOf` s || "undefined" `isInfixOf` s = assertFailure (printf "invalid instruction '%s' decoded to '%s'" s (showInstruction i))
ualMatches off Undefined s = assertBool "" True
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

{-
  0x00000f98: 0x912d1fcf smlawtls sp, pc, pc, r1: [Failed]
Failed: expected: "smlawtls sp, pc, pc, r1"
 but got: "teqls sp, pc, asr #31"

  0x00000e60: 0x731cf21e tstpvc ip, #-536870911: [Failed]
Failed: expected: "tstpvc ip, #-536870911"
 but got: "tstvc ip, #-536870911"

  0x00000e2c: 0x7006dcb6 strhvc sp, [r6], -r6: [Failed]
Failed: expected: "strhvc sp, [r6], -r6"
 but got: "andvc sp, r6, r6, lsr ip"


  0x00000954: 0xd0e9809f smlalle r8, r9, pc, r0: [Failed]
Failed: expected: "smlalle r8, r9, pc, r0"
 but got: "rscle r8, r9, pc, lsl r0"

  0x00000f54: 0x588c8018 stmpl ip, {r3, r4, pc}: [Failed]
Failed: expected: "stmpl ip, {r3, r4, pc}"
 but got: "stmpl ip, {r3, r4, pc}^"

-}