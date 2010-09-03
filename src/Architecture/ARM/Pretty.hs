module Architecture.ARM.Pretty where

-- TODO: update this later

{-
import Architecture.ARM.ARM
import Architecture.ARM.Thumb
import Architecture.ARM.Opcodes

import Text.PrettyPrint


showRegister R0 = "r0"
showRegister R1 = "r1"
showRegister R2 = "r2"
showRegister R3 = "r3"
showRegister R4 = "r4"
showRegister R5 = "r5"
showRegister R6 = "r6"
showRegister R7 = "r7"
showRegister R8 = "r8"
showRegister R9 = "r9"
showRegister R10 = "r10"
showRegister R11 = "r11"
showRegister R12 = "r12"
showRegister SP = "sp"
showRegister LR = "lr"
showRegister PC = "pc"


showArmShift S_LSL = "lsl"
showArmShift S_LSR = "lsr"
showArmShift S_ASR = "asr"
showArmShift S_ROR = "ror"

showArmCondition C_EQ = "eq"
showArmCondition C_NE = "ne"
showArmCondition C_CS = "cs"
showArmCondition C_CC = "cc"
showArmCondition C_MI = "mi"
showArmCondition C_PL = "pl"
showArmCondition C_VS = "vs"
showArmCondition C_VC = "vc"
showArmCondition C_HI = "hi"
showArmCondition C_LS = "ls"
showArmCondition C_GE = "ge"
showArmCondition C_LT = "lt"
showArmCondition C_GT = "gt"
showArmCondition C_LE = "le"
showArmCondition C_AL = ""
showArmCondition C_UND = "<unk>"


showWidth Byte = "b"
showWidth Halfword = "h"
showWidth Word = ""
showWidth Doubleword = "d"

showArmConditionalOpcode :: ARMConditionalOpcode -> String
showArmConditionalOpcode (O_B l addr) = printf "b%s%%s 0x%x" (if l then "l" else "") addr
showArmConditionalOpcode (O_BLX reg) = printf "blx%%s %s" (showRegister reg)
showArmConditionalOpcode (O_BX reg) = printf "bx%%s %s" (showRegister reg) 
showArmConditionalOpcode (O_BXJ reg) = printf "bxj%%s %s" (showRegister reg) 
showArmConditionalOpcode (O_AND s rd rn d) = printf "and%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_EOR s rd rn d) = printf "eor%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_SUB s rd rn d) = printf "sub%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_RSB s rd rn d) = printf "rsb%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_ADD s rd rn d) = printf "add%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_ADC s rd rn d) = printf "adc%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_SBC s rd rn d) = printf "sbc%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_RSC s rd rn d) = printf "rsc%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 

showArmConditionalOpcode (O_TST rn d) = printf "tst%%s %s, %s" (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_TEQ rn d) = printf "teq%%s %s, %s" (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_CMP rn d) = printf "cmp%%s %s, %s" (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_CMN rn d) = printf "cmn%%s %s, %s" (showRegister rn) (showArmOpData d) 

showArmConditionalOpcode (O_ORR s rd rn d) = printf "orr%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_MOV s rd d) = printf "mov%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 
showArmConditionalOpcode (O_LSL s rd d) = printf "lsl%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 
showArmConditionalOpcode (O_LSR s rd d) = printf "lsr%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 
showArmConditionalOpcode (O_ASR s rd d) = printf "asr%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 
showArmConditionalOpcode (O_RRX s rd d) = printf "rrx%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showRegister d) 
showArmConditionalOpcode (O_ROR s rd d) = printf "ror%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 
showArmConditionalOpcode (O_BIC s rd rn d) = printf "bic%%s%s %s, %s, %s" (if s then "s" else "") (showRegister rd) (showRegister rn) (showArmOpData d) 
showArmConditionalOpcode (O_MVN s rd d) = printf "mvn%%s%s %s, %s" (if s then "s" else "") (showRegister rd) (showArmOpData d) 

{-
| O_MLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
| O_MUL Bool ARMRegister ARMRegister ARMRegister 
| O_SMLA Nybble Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- SMLABB, SMLABT, SBMLATB, SMLATT
| O_SMLAD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMLAL (Maybe (Nybble, Nybble)) ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME: first two ArmRegisters are more complicated
| O_SMLALD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
| O_SMLAW Nybble ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMLSD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMLSLD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
| O_SMMLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMMUL Bool ARMRegister ARMRegister ARMRegister 
| O_SMMLS Bool ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMUAD Nybble ARMRegister ARMRegister ARMRegister
| O_SMUL Nybble Nybble ARMRegister ARMRegister ARMRegister
| O_SMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister
| O_SMULW Nybble ARMRegister ARMRegister ARMRegister
| O_SMUSD Nybble ARMRegister ARMRegister ARMRegister
| O_UMAAL ARMRegister ARMRegister ARMRegister ARMRegister
| O_UMLAL Bool ARMRegister ARMRegister ARMRegister ARMRegister
| O_UMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister

| O_QADD ARMRegister ARMRegister ARMRegister
| O_QADD16 ARMRegister ARMRegister ARMRegister
| O_QADD8 ARMRegister ARMRegister ARMRegister
| O_QADDSUBX ARMRegister ARMRegister ARMRegister
| O_QDADD ARMRegister ARMRegister ARMRegister
| O_QDSUB ARMRegister ARMRegister ARMRegister
| O_QSUB ARMRegister ARMRegister ARMRegister
| O_QSUB16 ARMRegister ARMRegister ARMRegister
| O_QSUB8 ARMRegister ARMRegister ARMRegister
| O_QSUBADDX ARMRegister ARMRegister ARMRegister

| O_SADD16 ARMRegister ARMRegister ARMRegister
| O_SADD8 ARMRegister ARMRegister ARMRegister
| O_SADDSUBX ARMRegister ARMRegister ARMRegister
| O_SSUB16 ARMRegister ARMRegister ARMRegister
| O_SSUB8 ARMRegister ARMRegister ARMRegister
| O_SSUBADDX ARMRegister ARMRegister ARMRegister

| O_SHADD16 ARMRegister ARMRegister ARMRegister
| O_SHADD8 ARMRegister ARMRegister ARMRegister
| O_SHADDSUBX ARMRegister ARMRegister ARMRegister
| O_SHSUB16 ARMRegister ARMRegister ARMRegister
| O_SHSUB8 ARMRegister ARMRegister ARMRegister
| O_SHSUBADDX ARMRegister ARMRegister ARMRegister

| O_UADD16 ARMRegister ARMRegister ARMRegister
| O_UADD8 ARMRegister ARMRegister ARMRegister
| O_UADDSUBX ARMRegister ARMRegister ARMRegister
| O_USUB16 ARMRegister ARMRegister ARMRegister
| O_USUB8 ARMRegister ARMRegister ARMRegister
| O_USUBADDX ARMRegister ARMRegister ARMRegister

| O_UHADD16 ARMRegister ARMRegister ARMRegister
| O_UHADD8 ARMRegister ARMRegister ARMRegister
| O_UHADDSUBX ARMRegister ARMRegister ARMRegister
| O_UHSUB16 ARMRegister ARMRegister ARMRegister
| O_UHSUB8 ARMRegister ARMRegister ARMRegister
| O_UHSUBADDX ARMRegister ARMRegister ARMRegister

| O_UQADD16 ARMRegister ARMRegister ARMRegister
| O_UQADD8 ARMRegister ARMRegister ARMRegister
| O_UQADDSUBX ARMRegister ARMRegister ARMRegister
| O_UQSUB16 ARMRegister ARMRegister ARMRegister
| O_UQSUB8 ARMRegister ARMRegister ARMRegister
| O_UQSUBADDX ARMRegister ARMRegister ARMRegister

| O_SXTAB16 ARMRegister ARMRegister ARMOpData -- rotate
| O_SXTAB ARMRegister ARMRegister ARMOpData -- rotate
| O_SXTAH ARMRegister ARMRegister ARMOpData -- rotate
| O_SXTB16 ARMRegister ARMOpData -- rotate
| O_SXT Width ARMRegister ARMOpData --rotate only -- ONLY SXTB, SXTH
| O_UXTAB16 ARMRegister ARMRegister ARMOpData -- rotate 
| O_UXTAB ARMRegister ARMRegister ARMOpData -- rotate
| O_UXTAH ARMRegister ARMRegister ARMOpData -- rotate
| O_UXTB16 ARMRegister ARMOpData -- rotate
| O_UXT Width ARMRegister ARMOpData -- rotate -- UXTB, UXTH

| O_CLZ ARMRegister ARMRegister
| O_USAD8 ARMRegister ARMRegister ARMRegister 
| O_USADA8 ARMRegister ARMRegister ARMRegister ARMRegister
| O_PKHBT ARMRegister ARMRegister ARMOpData -- rotate/shift
| O_PKHTB ARMRegister ARMRegister ARMOpData -- rotate/shift
| O_REV ARMRegister ARMRegister
| O_REV16 ARMRegister ARMRegister
| O_REVSH ARMRegister ARMRegister
| O_SEL ARMRegister ARMRegister ARMRegister
| O_SSAT ARMRegister Word8 ARMOpData -- rotate/shift
| O_SSAT16 ARMRegister Word8 ARMRegister
| O_USAT ARMRegister Word8 ARMOpData -- rotate/shift
| O_USAT16 ARMRegister Word8 ARMRegister

| O_MRS ARMRegister ARMStatusRegister
| O_MSR 
-}

showArmConditionalOpcode (O_LDR w t s rd m) = printf "ldr%s%s%s%%s %s, %s" (if s then "s" else "") (showWidth w) (if t then "t" else "") (showRegister rd) (showArmOpMemory m)
showArmConditionalOpcode (O_STR w t s rd m) = printf "str%s%s%s%%s %s, %s" (if s then "s" else "") (showWidth w) (if t then "t" else "") (showRegister rd) (showArmOpMemory m)

-- | O_LDREX
-- | O_STREX
   
-- | O_LDM -- Note that there are three different forms
-- | O_STM -- Note that there are two different forms
-- | O_PUSH [ARMRegister]
-- | O_POP [ARMRegister]


showArmConditionalOpcode _ = "unknown %s"


showArmOpData :: ARMOpData -> String
showArmOpData (OP_Imm i) = printf "#%d" i
showArmOpData (OP_Reg reg) = showRegister reg
showArmOpData (OP_RegShiftImm sh i reg) = printf "%s, %s #%d" (showRegister reg) (showArmShift sh) i
showArmOpData (OP_RegShiftReg sh regs reg) = printf "%s, %s %s" (showRegister reg) (showArmShift sh) (showRegister regs)
showArmOpData (OP_RegShiftRRX reg) = printf "%s, RRX" (showRegister reg)

showArmOpMemory :: ARMOpMemory -> String
showArmOpMemory (OP_MemReg base d up) = (printf "[%s, %s]" (showRegister base) (showArmOpData d)) ++ if up then "!" else ""
showArmOpMemory (OP_MemRegNeg base d up) = (printf "[%s, -%s]" (showRegister base) (showArmOpData d)) ++ if up then "!" else ""
showArmOpMemory (OP_MemRegPost base d) = printf "[%s], %s" (showRegister base) (showArmOpData d)
showArmOpMemory (OP_MemRegPostNeg base d) = printf "[%s], -%s" (showRegister base) (showArmOpData d)


showArmOpMultiple :: ARMOpMultiple -> String
showArmOpMultiple (OP_Regs rs) = "{" ++ (intercalate ", " . map showRegister $ rs) ++ "}"
showArmOpMultiple (OP_RegsCaret rs) = "{" ++ (intercalate ", " . map showRegister $ rs) ++ "}^"


showArmInstruction (ARMUnconditionalInstruction x) = undefined --showArmUnconditionalOpcode x
showArmInstruction (ARMConditionalInstruction cond x) = printf (showArmConditionalOpcode x) (showArmCondition cond)
-}