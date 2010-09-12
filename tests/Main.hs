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
ualMatches off x ('s':'t':'m':'i':'a':r) = ualMatches off x $ 's':'t':'m':r
ualMatches off x ('l':'d':'m':'i':'a':r) = ualMatches off x $ 'l':'d':'m':r
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


  0x00000d14: 0x91e77c96 strbls r7, [r7, #198]!: [Failed]
Failed: expected: "strbls r7, [r7, #198]!"
 but got: "mvnls r7, r6, lsl ip"

  0x00000f88: 0x504f19b5 strhpl r1, [pc, #-149]: [Failed]
Failed: expected: "strhpl r1, [pc, #-149]"
 but got: "strhpl r1, [pc], #-149"
 
   0x00000f74: 0xe0ff0bdc ldrsbt r0, [pc, #188]: [Failed]
 Failed: expected: "ldrsbt r0, [pc, #188]"
  but got: "ldrsbt r0, [pc], #188"

  0x00000ea0: 0xb142f994 strblt pc, [r2, #-148]: [Failed]
Failed: expected: "strblt pc, [r2, #-148]"
 but got: "cmplt r2, r4, lsl r9"

  0x00000cb4: 0x9374fd0d cmnpls r4, #832: [Failed]
Failed: expected: "cmnpls r4, #832"
 but got: "cmnls r4, #832"

  0x00009a40: 0xc094dbbe ldrhgt sp, [r4], lr: [Failed]
Failed: expected: "ldrhgt sp, [r4], lr"
 but got: "addsgt sp, r4, lr, lsr fp"

  0x0000994c: 0x702d01be strhvc r0, [sp], -lr: [Failed]
Failed: expected: "strhvc r0, [sp], -lr"
 but got: "eorvc r0, sp, lr, lsr r1"

  0x00009664: 0xe1b0fb1a lsls pc, sl, fp: [Failed]
Failed: expected: "lsls pc, sl, fp"
 but got: "lsls pc, sl, lsl fp"

  0x00009578: 0xa1ff98b6 ldrhge r9, [pc, #134]: [Failed]
Failed: expected: "ldrhge r9, [pc, #134]"
 but got: "ldrhge r9, [pc, #134]!"
-}