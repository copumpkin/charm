{-# LANGUAGE GADTs #-}
module Architecture.ARM.Pretty where

import Prelude hiding (EQ, LT, GT)

import Architecture.ARM.Common
import Architecture.ARM.Instructions.UAL

import Text.Printf
import Text.PrettyPrint

import Data.List

-- All this string building is inefficient. Fix it sometime using a real prettyprinting library

showRegister R0  = "R0"
showRegister R1  = "R1"
showRegister R2  = "R2"
showRegister R3  = "R3"
showRegister R4  = "R4"
showRegister R5  = "R5"
showRegister R6  = "R6"
showRegister R7  = "R7"
showRegister R8  = "R8"
showRegister R9  = "R9"
showRegister R10 = "SL"
showRegister R11 = "FP"
showRegister R12 = "IP"
showRegister SP  = "SP"
showRegister LR  = "LR"
showRegister PC  = "PC"

showArmShift S_LSL = "LSL"
showArmShift S_LSR = "LSR"
showArmShift S_ASR = "ASR"
showArmShift S_ROR = "ROR"

showCondition EQ = "EQ"
showCondition NE = "NE"
showCondition CS = "CS"
showCondition CC = "CC"
showCondition MI = "MI"
showCondition PL = "PL"
showCondition VS = "VS"
showCondition VC = "VC"
showCondition HI = "HI"
showCondition LS = "LS"
showCondition GE = "GE"
showCondition LT = "LT"
showCondition GT = "GT"
showCondition LE = "LE"
showCondition AL = ""
showCondition UND = "<unk>"


showDataOp :: DataOp -> String
showDataOp (Imm i) = printf "#%d" i
showDataOp (Reg reg) = showRegister reg
showDataOp (RegShiftImm sh i reg) = printf "%s, %s #%d" (showRegister reg) (showArmShift sh) i
showDataOp (RegShiftReg sh regs reg) = printf "%s, %s %s" (showRegister reg) (showArmShift sh) (showRegister regs)
showDataOp (RegShiftRRX reg) = printf "%s, RRX" (showRegister reg)

showMemOp :: MemOp -> String
showMemOp (MemReg base (Imm 0) up) = printf "[%s]" (showRegister base) ++ if up then "!" else ""
showMemOp (MemReg base d up) = printf "[%s, %s]" (showRegister base) (showDataOp d) ++ if up then "!" else ""
showMemOp (MemRegNeg base d up) = printf "[%s, -%s]" (showRegister base) (showDataOp d) ++ if up then "!" else ""
showMemOp (MemRegPost base d) = printf "[%s], %s" (showRegister base) (showDataOp d)
showMemOp (MemRegPostNeg base d) = printf "[%s], -%s" (showRegister base) (showDataOp d)


showMultiRegOp :: MultiRegOp -> String
showMultiRegOp (Regs rs) = "{" ++ (intercalate ", " . map showRegister $ rs) ++ "}"
showMultiRegOp (RegsCaret rs) = "{" ++ (intercalate ", " . map showRegister $ rs) ++ "}^"

showInstruction :: Instruction UAL c -> String
showInstruction (B off)                    = printf "B%%s 0x%x" off
showInstruction (BL off)                   = printf "BL%%s 0x%x" off
showInstruction (BLX d)                    = printf "BLX%%s %s" (showDataOp d)
showInstruction (BX  rd)                   = printf "BX%%s %s"  (showRegister rd)
showInstruction (BXJ rd)                   = printf "BXJ%%s %s" (showRegister rd)
showInstruction (AND  rd rn k) | rd == rn  = printf "AND%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "AND%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ANDS rd rn k) | rd == rn  = printf "ANDS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "ANDS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (EOR  rd rn k) | rd == rn  = printf "EOR%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "EOR%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (EORS rd rn k) | rd == rn  = printf "EORS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "EORS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (SUB  rd rn k) | rd == rn  = printf "SUB%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "SUB%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (SUBS rd rn k) | rd == rn  = printf "SUBS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "SUBS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (RSB  rd rn k) | rd == rn  = printf "RSB%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "RSB%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (RSBS rd rn k) | rd == rn  = printf "RSBS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "RSBS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ADD  rd rn k) | rd == rn  = printf "ADD%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "ADD%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ADDS rd rn k) | rd == rn  = printf "ADDS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "ADDS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ADC  rd rn k) | rd == rn  = printf "ADC%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "ADC%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ADCS rd rn k) | rd == rn  = printf "ADCS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "ADCS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (SBC  rd rn k) | rd == rn  = printf "SBC%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "SBC%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (SBCS rd rn k) | rd == rn  = printf "SBCS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "SBCS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (RSC  rd rn k) | rd == rn  = printf "RSC%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "RSC%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (RSCS rd rn k) | rd == rn  = printf "RSCS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "RSCS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ORR  rd rn k) | rd == rn  = printf "ORR%%s %s, %s"      (showRegister rd) (showDataOp k)
                               | otherwise = printf "ORR%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp k)
showInstruction (ORRS rd rn k) | rd == rn  = printf "ORRS%%s %s, %s"     (showRegister rd) (showDataOp k)
                               | otherwise = printf "ORRS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp k)

showInstruction (TST rn d)            = printf "TST%%s %s, %s" (showRegister rn) (showDataOp d)
showInstruction (TEQ rn d)            = printf "TEQ%%s %s, %s" (showRegister rn) (showDataOp d)
showInstruction (CMP rn d)            = printf "CMP%%s %s, %s" (showRegister rn) (showDataOp d)
showInstruction (CMN rn d)            = printf "CMN%%s %s, %s" (showRegister rn) (showDataOp d)
showInstruction (MOV  rd d)           = printf "MOV%%s %s, %s"  (showRegister rd) (showDataOp d)
showInstruction (MOVS rd d)           = printf "MOVS%%s %s, %s" (showRegister rd) (showDataOp d)

showInstruction (LSL  rd (RegShiftImm S_LSL i r)) = printf "LSL%%s %s, %s, #%i"  (showRegister rd) (showRegister r) i
showInstruction (LSL  rd d)                       = printf "LSL%%s %s, %s"       (showRegister rd) (showDataOp d)

showInstruction (LSLS rd (RegShiftImm S_LSL i r)) = printf "LSLS%%s %s, %s, #%i" (showRegister rd) (showRegister r) i
showInstruction (LSLS rd d)                       = printf "LSLS%%s %s, %s"      (showRegister rd) (showDataOp d)

showInstruction (LSR  rd (RegShiftImm S_LSR i r)) = printf "LSR%%s %s, %s, #%i"  (showRegister rd) (showRegister r) i
showInstruction (LSR  rd d)                       = printf "LSR%%s %s, %s"       (showRegister rd) (showDataOp d)

showInstruction (LSRS rd (RegShiftImm S_LSR i r)) = printf "LSRS%%s %s, %s, #%i" (showRegister rd) (showRegister r) i
showInstruction (LSRS rd d)                       = printf "LSRS%%s %s, %s"      (showRegister rd) (showDataOp d)

showInstruction (ASR  rd (RegShiftImm S_ASR i r)) = printf "ASR%%s %s, %s, #%i"  (showRegister rd) (showRegister r) i
showInstruction (ASR  rd d)                       = printf "ASR%%s %s, %s"       (showRegister rd) (showDataOp d)

showInstruction (ASRS rd (RegShiftImm S_ASR i r)) = printf "ASRS%%s %s, %s, #%i" (showRegister rd) (showRegister r) i
showInstruction (ASRS rd d)                       = printf "ASRS%%s %s, %s"      (showRegister rd) (showDataOp d)

showInstruction (ROR  rd (RegShiftImm S_ROR i r)) = printf "ROR%%s %s, %s, #%i"  (showRegister rd) (showRegister r) i
showInstruction (ROR  rd d)                       = printf "ROR%%s %s, %s"       (showRegister rd) (showDataOp d)

showInstruction (RORS rd (RegShiftImm S_ROR i r)) = printf "RORS%%s %s, %s, #%i" (showRegister rd) (showRegister r) i
showInstruction (RORS rd d)                       = printf "RORS%%s %s, %s"      (showRegister rd) (showDataOp d)

showInstruction (RRX  rd rm)          = printf "RRX%%s %s, %s"  (showRegister rd) (showRegister rm)
showInstruction (RRXS rd rm)          = printf "RRXS%%s %s, %s" (showRegister rd) (showRegister rm)
showInstruction (BIC  rd rn d)        = printf "BIC%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (BICS rd rn d)        = printf "BICS%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (MVN  rd d)           = printf "MVN%%s %s, %s"  (showRegister rd) (showDataOp d)
showInstruction (MVNS rd d)           = printf "MVNS%%s %s, %s" (showRegister rd) (showDataOp d)
showInstruction (MLA  rd rn rm ra)    = printf "MLA%%s %s, %s, %s, %s"     (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (MLAS rd rn rm ra)    = printf "MLAS%%s %s, %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (MUL  rd rn rm)       = printf "MUL%%s %s, %s, %s"     (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (MULS rd rn rm)       = printf "MULS%%s %s, %s, %s"     (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMLABB  rd rn rm ra) = printf "SMLABB%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLABT  rd rn rm ra) = printf "SMLABT%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLATB  rd rn rm ra) = printf "SMLATB%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLATT  rd rn rm ra) = printf "SMLATT%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLAWB  rd rn rm ra) = printf "SMLAWB%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLAWT  rd rn rm ra) = printf "SMLAWT%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLAD   rd rn rm ra) = printf "SMLAD%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLADX  rd rn rm ra) = printf "SMLADX%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLAL   rd rn rm ra) = printf "SMLAL%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALS  rd rn rm ra) = printf "SMLALS%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALBB rd rn rm ra) = printf "SMLALBB%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALBT rd rn rm ra) = printf "SMLALBT%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALTB rd rn rm ra) = printf "SMLALTB%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALTT rd rn rm ra) = printf "SMLALTT%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALD  rd rn rm ra) = printf "SMLALD%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLALDX rd rn rm ra) = printf "SMLALDX%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLSD   rd rn rm ra) = printf "SMLSD%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLSDX  rd rn rm ra) = printf "SMLSDX%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLSLD  rd rn rm ra) = printf "SMLSLD%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMLSLDX rd rn rm ra) = printf "SMLSLDX%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMMLA   rd rn rm ra) = printf "SMMLA%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMMLAR  rd rn rm ra) = printf "SMMLAR%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMMUL   rd rn rm)    = printf "SMMUL%%s %s, %s, %s"       (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMMULR  rd rn rm)    = printf "SMMULR%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMMLS   rd rn rm ra) = printf "SMMLS%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMMLSR  rd rn rm ra) = printf "SMMLSR%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMUAD   rd rn rm)    = printf "SMUAD%%s %s, %s, %s"       (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMUADX  rd rn rm)    = printf "SMUADX%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULBB  rd rn rm)    = printf "SMULBB%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULBT  rd rn rm)    = printf "SMULBT%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULTB  rd rn rm)    = printf "SMULTB%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULTT  rd rn rm)    = printf "SMULTT%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULL   rd rn rm ra) = printf "SMULL%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMULLS  rd rn rm ra) = printf "SMULLS%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (SMULWB  rd rn rm)    = printf "SMULWB%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMULWT  rd rn rm)    = printf "SMULWT%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMUSD   rd rn rm)    = printf "SMUSD%%s %s, %s, %s"       (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SMUSDX  rd rn rm)    = printf "SMUSDX%%s %s, %s, %s"      (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (UMAAL   rd rn rm ra) = printf "UMAAL%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (UMLAL   rd rn rm ra) = printf "UMLAL%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (UMLALS  rd rn rm ra) = printf "UMLALS%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (UMULL   rd rn rm ra) = printf "UMULL%%s %s, %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (UMULLS  rd rn rm ra) = printf "UMULLS%%s %s, %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (QADD    rd rm rn)    = printf "QADD%%s %s, %s, %s"    (showRegister rd) (showRegister rm) (showRegister rn) -- NB: inverted operands
showInstruction (QADD16  rd rn rm)    = printf "QADD16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QADD8   rd rn rm)    = printf "QADD8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QASX    rd rn rm)    = printf "QASX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QDADD   rd rn rm)    = printf "QDADD%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QDSUB   rd rn rm)    = printf "QDSUB%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QSUB    rd rm rn)    = printf "QSUB%%s %s, %s, %s"    (showRegister rd) (showRegister rm) (showRegister rn) -- NB: inverted operands
showInstruction (QSUB16  rd rn rm)    = printf "QSUB16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QSUB8   rd rn rm)    = printf "QSUB8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (QSAX    rd rn rm)    = printf "QSAX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SADD16  rd rn rm)    = printf "SADD16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SADD8   rd rn rm)    = printf "SADD8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SASX    rd rn rm)    = printf "SASX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SSUB16  rd rn rm)    = printf "SSUB16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SSUB8   rd rn rm)    = printf "SSUB8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SSAX    rd rn rm)    = printf "SSAX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHADD16 rd rn rm)    = printf "SHADD16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHADD8  rd rn rm)    = printf "SHADD8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHASX   rd rn rm)    = printf "SHASX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHSUB16 rd rn rm)    = printf "SHSUB16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHSUB8  rd rn rm)    = printf "SHSUB8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SHSAX   rd rn rm)    = printf "SHSAX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UADD16  rd rn rm)    = printf "UADD16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UADD8   rd rn rm)    = printf "UADD8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UASX    rd rn rm)    = printf "UASX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (USUB16  rd rn rm)    = printf "USUB16%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (USUB8   rd rn rm)    = printf "USUB8%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (USAX    rd rn rm)    = printf "USAX%%s %s, %s, %s"    (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHADD16 rd rn rm)    = printf "UHADD16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHADD8  rd rn rm)    = printf "UHADD8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHASX   rd rn rm)    = printf "UHASX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHSUB16 rd rn rm)    = printf "UHSUB16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHSUB8  rd rn rm)    = printf "UHSUB8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UHSAX   rd rn rm)    = printf "UHSAX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQADD16 rd rn rm)    = printf "UQADD16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQADD8  rd rn rm)    = printf "UQADD8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQASX   rd rn rm)    = printf "UQASX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQSUB16 rd rn rm)    = printf "UQSUB16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQSUB8  rd rn rm)    = printf "UQSUB8%%s %s, %s, %s"  (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (UQSAX   rd rn rm)    = printf "UQSAX%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showRegister rm) 
showInstruction (SXTAB16 rd rn d)     = printf "SXTAB16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp d) 
showInstruction (SXTAB   rd rn d)     = printf "SXTAB%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showDataOp d) 
showInstruction (SXTAH   rd rn d)     = printf "SXTAH%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showDataOp d) 
showInstruction (SXTB16  rd d)        = printf "SXTB16%%s %s, %s"      (showRegister rd) (showDataOp d)
showInstruction (SXTB    rd d)        = printf "SXTB%%s %s, %s"        (showRegister rd) (showDataOp d)
showInstruction (SXTH    rd d)        = printf "SXTH%%s %s, %s"        (showRegister rd) (showDataOp d)
showInstruction (UXTAB16 rd rn d)     = printf "UXTAB16%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (UXTAB   rd rn d)     = printf "UXTAB%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (UXTAH   rd rn d)     = printf "UXTAH%%s %s, %s, %s"   (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (UXTB16  rd d)        = printf "UXTB16%%s %s, %s"      (showRegister rd) (showDataOp d)
showInstruction (UXTB    rd d)        = printf "UXTB%%s %s, %s"        (showRegister rd) (showDataOp d)
showInstruction (UXTH    rd d)        = printf "UXTH%%s %s, %s"        (showRegister rd) (showDataOp d)
showInstruction (UBFX    rd rn lsb w) = printf "UBFX%%s %s, %s, #%i, #%i" (showRegister rd) (showRegister rn) lsb w
showInstruction (SBFX    rd rn lsb w) = printf "SBFX%%s %s, %s, #%i, #%i" (showRegister rd) (showRegister rn) lsb w
showInstruction (CLZ rd rm)           = printf "CLZ%%s %s, %s" (showRegister rd) (showRegister rm)
showInstruction (USAD8  rd rn rm)     = printf "USAD8%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (USADA8 rd rn rm ra)  = printf "USADA8%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (PKHBT rd rn d)       = printf "PKHBT%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (PKHTB rd rn d)       = printf "PKHTB%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showDataOp d)
showInstruction (REV   rd rm)         = printf "REV%%s %s, %s" (showRegister rd) (showRegister rm)
showInstruction (REV16 rd rm)         = printf "REV16%%s %s, %s" (showRegister rd) (showRegister rm)
showInstruction (REVSH rd rm)         = printf "REVSH%%s %s, %s" (showRegister rd) (showRegister rm)
showInstruction (SEL rd rn rm)        = printf "SEL%%s %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm)
showInstruction (SSAT   rd imm d)     = printf "SSAT%%s %s, #%i, %s"   (showRegister rd) imm (showDataOp d)
showInstruction (SSAT16 rd imm rn)    = printf "SSAT16%%s %s, #%i, %s" (showRegister rd) imm (showRegister rn)
showInstruction (USAT   rd imm d)     = printf "USAT%%s %s, #%i, %s"   (showRegister rd) imm (showDataOp d)
showInstruction (USAT16 rd imm rn)    = printf "USAT16%%s %s, #%i, %s" (showRegister rd) imm (showRegister rn)
showInstruction (MRS rd spec)         = printf "MRS%%s %s, %s" (showRegister rd) (show spec) -- check
showInstruction (MSR x y d)           = printf "MSR%%s" -- undefined -- FIX
showInstruction (LDR    rt mem)       = printf "LDR%%s %s, %s"    (showRegister rt) (showMemOp mem)
showInstruction (LDRB   rt mem)       = printf "LDRB%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (LDRH   rt mem)       = printf "LDRH%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (LDRD   rt mem)       = printf "LDRD%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (LDRBT  rt mem)       = printf "LDRBT%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (LDRHT  rt mem)       = printf "LDRHT%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (LDRT   rt mem)       = printf "LDRT%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (LDRSB  rt mem)       = printf "LDRSB%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (LDRSBT rt mem)       = printf "LDRSBT%%s %s, %s" (showRegister rt) (showMemOp mem)
showInstruction (LDRSH  rt mem)       = printf "LDRSH%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (LDRSHT rt mem)       = printf "LDRSHT%%s %s, %s" (showRegister rt) (showMemOp mem)
showInstruction (STR    rt mem)       = printf "STR%%s %s, %s"    (showRegister rt) (showMemOp mem)
showInstruction (STRB   rt mem)       = printf "STRB%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (STRH   rt mem)       = printf "STRH%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (STRD   rt mem)       = printf "STRD%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (STRBT  rt mem)       = printf "STRBT%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (STRHT  rt mem)       = printf "STRHT%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (STRT   rt mem)       = printf "STRT%%s %s, %s"   (showRegister rt) (showMemOp mem)
showInstruction (LDREX  rt mem)       = printf "LDREX%%s %s, %s"  (showRegister rt) (showMemOp mem)
showInstruction (LDREXB rt mem)       = printf "LDREXB%%s %s, %s" (showRegister rt) (showMemOp mem)
showInstruction (LDREXH rt mem)       = printf "LDREXH%%s %s, %s" (showRegister rt) (showMemOp mem)
showInstruction (LDREXD rt rt2 m)     = printf "LDREXD%%s %s, %s, %s" (showRegister rt) (showRegister rt2) (showMemOp m)
showInstruction (STREX  rd rt m)      = printf "STREX%%s %s, %s, %s"  (showRegister rd) (showRegister rt) (showMemOp m)
showInstruction (STREXB rd rt m)      = printf "STREXB%%s %s, %s, %s" (showRegister rd) (showRegister rt) (showMemOp m)
showInstruction (STREXH rd rt m)      = printf "STREXH%%s %s, %s, %s" (showRegister rd) (showRegister rt) (showMemOp m)
showInstruction (STREXD rd rt rt2 m)  = printf "STREXD%%s %s, %s, %s" (showRegister rd) (showRegister rt) (showRegister rt2) (showMemOp m)
showInstruction (LDM   bang rd regs)  = printf "LDM%%s %s%s, %s"   (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (LDMDA bang rd regs)  = printf "LDMDA%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (LDMDB bang rd regs)  = printf "LDMDB%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (LDMIB bang rd regs)  = printf "LDMIB%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (STM   bang rd regs)  = printf "STM%%s %s%s, %s"   (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (STMDA bang rd regs)  = printf "STMDA%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (STMDB bang rd regs)  = printf "STMDB%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (STMIB bang rd regs)  = printf "STMIB%%s %s%s, %s" (showRegister rd) (cond "" "!" bang) (showMultiRegOp regs)
showInstruction (PUSH regs)           = printf "PUSH%%s %s" (showMultiRegOp regs)
showInstruction (POP  regs)           = printf "POP%%s %s"  (showMultiRegOp regs)
showInstruction (SWP  rt rt2 rn)      = printf "SWP%%s %s, %s, %s"  (showRegister rt) (showRegister rt2) (showMemOp rn)
showInstruction (SWPB rt rt2 rn)      = printf "SWPB%%s %s, %s, %s" (showRegister rt) (showRegister rt2) (showMemOp rn)
showInstruction (SMC imm)             = printf "SMC%%s 0x%x" imm
showInstruction (SVC imm)             = printf "SVC%%s 0x%08x" imm
showInstruction (DBG imm)             = printf "DBG%%s 0x%x" imm
showInstruction (DMB opt)             = printf "DMB%%s %s" (show opt) -- FIX
showInstruction (DSB opt)             = printf "DSB%%s %s" (show opt) -- FIX
showInstruction (ISB opt)             = printf "ISB%%s %s" (show opt) -- FIX
showInstruction (PLI mem)             = printf "PLI%%s %s" (showMemOp mem)
showInstruction YIELD                 = printf "YIELD"
showInstruction WFE                   = printf "WFE"
showInstruction WFI                   = printf "WFI"
showInstruction SEV                   = printf "SEV"
showInstruction (BFC rd x)            = printf "BFC%%s %s, %s" (showRegister rd) "DUNNO YET" -- FIX
showInstruction (BFI rd rn x)         = printf "BFI%%s %s, %s, %s" (showRegister rd) (showRegister rn) "DUNNO YET" -- FIX
showInstruction (MLS rd rn rm ra)     = printf "MLS%%s %s, %s, %s, %s" (showRegister rd) (showRegister rn) (showRegister rm) (showRegister ra)
showInstruction (MOVW rd imm)         = printf "MOVW%%s %s, #%i" (showRegister rd) imm
showInstruction (MOVT rd imm)         = printf "MOVT%%s %s, #%i" (showRegister rd) imm
showInstruction (RBIT rd rm)          = "rbit%s" -- undefined -- FIX

showInstruction (CPS imm32)           = printf "CPS 0x%x" imm32
showInstruction (CPSIE a i f mode)    = printf "CPSIE" -- FIX
showInstruction (CPSID a i f mode)    = printf "CPSIE" -- FIX
showInstruction (SETEND end)          = printf "SETEND %s" (show end) -- FIX
showInstruction (RFE   b r)           = printf "RFE"   -- FIX
showInstruction (RFEDA b r)           = printf "RFEDA" -- FIX
showInstruction (RFEDB b r)           = printf "RFEDB" -- FIX
showInstruction (RFEIB b r)           = printf "RFEIB" -- FIX
showInstruction (BKPT imm8)           = printf "BKPT 0x%x" imm8
showInstruction (PLD mem)             = "PLD" -- FIX
showInstruction (SRS   b r imm32)     = "SRS" -- FIX
showInstruction (SRSDA b r imm32)     = "SRSDA" -- FIX
showInstruction (SRSDB b r imm32)     = "SRSDB" -- FIX
showInstruction (SRSIB b r imm32)     = "SRSIB" -- FIX
showInstruction CLREX                 = "CLREX"
showInstruction i                     = error "dunno how to print instruction '" ++ (show i) ++ "'"

showGeneralInstruction :: GeneralInstruction UAL -> String
showGeneralInstruction Undefined = "UNDEFINED INSTRUCTION"
showGeneralInstruction (Unconditional x) = showInstruction x
showGeneralInstruction (Conditional cond x) = printf (showInstruction x) (showCondition cond)