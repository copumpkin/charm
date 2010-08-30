module Architecture.ARM.Instructions where

import Architecture.ARM.Common

import Data.Int
import Data.Word

{-
data ARMOpRegister = Reg ARMRegister
                   | RegBang ARMRegister
  deriving (Show, Read, Eq)
-}

data ARMOpData = Imm Int
               | Reg ARMRegister
               | RegShiftImm ARMShift Int ARMRegister 
               | RegShiftReg ARMShift ARMRegister ARMRegister
               | RegShiftRRX ARMRegister
  deriving (Show, Read, Eq)
  
data ARMOpMemory = MemReg ARMRegister ARMOpData Bool
                 | MemRegNeg ARMRegister ARMOpData Bool
                 | MemRegPost ARMRegister ARMOpData
                 | MemRegPostNeg ARMRegister ARMOpData
  deriving (Show, Read, Eq)

data ARMOpMultiple = Regs [ARMRegister]
                   | RegsCaret [ARMRegister]
  deriving (Show, Read, Eq)

data ARMInstruction = ARMUnconditionalInstruction ARMUnconditionalOpcode
                    | ARMConditionalInstruction ARMCondition ARMConditionalOpcode
  deriving (Show, Read, Eq)

data ARMConditionalOpcode = B Bool Int32 -- B, BL
                          | BL Int32 -- NEW
                          | BLX ARMRegister -- conditional form
                          | BX ARMRegister
                          | BXJ ARMRegister
                          
                          | AND  Bool ARMRegister ARMRegister ARMOpData -- AND, ANDS
                          | ANDS ARMRegister ARMRegister ARMOpData -- NEW
                          | EOR  Bool ARMRegister ARMRegister ARMOpData -- EOR, EORS
                          | EORS ARMRegister ARMRegister ARMOpData -- NEW
                          | SUB  Bool ARMRegister ARMRegister ARMOpData -- SUB, SUBS
                          | SUBS ARMRegister ARMRegister ARMOpData -- NEW
                          | RSB  Bool ARMRegister ARMRegister ARMOpData -- RSB, RSBS
                          | RSBS ARMRegister ARMRegister ARMOpData -- NEW
                          | ADD  Bool ARMRegister ARMRegister ARMOpData -- ADD, ADDS
                          | ADDS ARMRegister ARMRegister ARMOpData -- NEW
                          | ADC  Bool ARMRegister ARMRegister ARMOpData -- ADC, ADCS
                          | ADCS ARMRegister ARMRegister ARMOpData -- NEW
                          | SBC  Bool ARMRegister ARMRegister ARMOpData -- SBC, SBCS
                          | SBCS ARMRegister ARMRegister ARMOpData -- NEW
                          | RSC  Bool ARMRegister ARMRegister ARMOpData -- RSC, RSCS
                          | RSCS ARMRegister ARMRegister ARMOpData -- NEW
                          | TST ARMRegister ARMOpData -- TST
                          | TEQ ARMRegister ARMOpData -- TEQ
                          | CMP ARMRegister ARMOpData -- CMP
                          | CMN ARMRegister ARMOpData -- CMN
                          | ORR  Bool ARMRegister ARMRegister ARMOpData -- ORR, ORRS
                          | ORRS ARMRegister ARMRegister ARMOpData -- NEW
                          | MOV  Bool ARMRegister ARMOpData -- MOV, MOVS
                          | MOVS ARMRegister ARMOpData -- NEW
                          | LSL  Bool ARMRegister ARMOpData
                          | LSLS ARMRegister ARMOpData -- NEW
                          | LSR  Bool ARMRegister ARMOpData
                          | LSRS ARMRegister ARMOpData -- NEW
                          | ASR  Bool ARMRegister ARMOpData
                          | ASRS ARMRegister ARMOpData -- NEW
                          | RRX  Bool ARMRegister ARMRegister 
                          | RRXS ARMRegister ARMRegister -- NEW
                          | ROR  Bool ARMRegister ARMOpData
                          | RORS ARMRegister ARMOpData -- NEW
                          | BIC  Bool ARMRegister ARMRegister ARMOpData -- BIC, BICS
                          | BICS ARMRegister ARMRegister ARMOpData -- NEW
                          | MVN  Bool ARMRegister ARMOpData -- MVN, MVNS
                          | MVNS ARMRegister ARMOpData -- NEW
                                                    
                          | MLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | MUL Bool ARMRegister ARMRegister ARMRegister 
                          | SMLA Nybble Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- SMLABB, SMLABT, SBMLATB, SMLATT
                          | SMLABB ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLABT ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLATB ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLATT ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLAD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLAL (Maybe (Nybble, Nybble)) ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME: first two ArmRegisters are more complicated
                          | SMLALD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
                          | SMLAW Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLSD Nybble ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLSLD Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME as above
                          | SMMLA Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMMUL Bool ARMRegister ARMRegister ARMRegister 
                          | SMMLS Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMUAD Nybble ARMRegister ARMRegister ARMRegister
                          | SMUL Nybble Nybble ARMRegister ARMRegister ARMRegister
                          | SMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMULW Nybble ARMRegister ARMRegister ARMRegister
                          | SMUSD Nybble ARMRegister ARMRegister ARMRegister
                          | UMAAL ARMRegister ARMRegister ARMRegister ARMRegister
                          | UMLAL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          | UMULL Bool ARMRegister ARMRegister ARMRegister ARMRegister
                          
                          | QADD ARMRegister ARMRegister ARMRegister
                          | QADD16 ARMRegister ARMRegister ARMRegister
                          | QADD8 ARMRegister ARMRegister ARMRegister
                          | QADDSUBX ARMRegister ARMRegister ARMRegister
                          | QDADD ARMRegister ARMRegister ARMRegister
                          | QDSUB ARMRegister ARMRegister ARMRegister
                          | QSUB ARMRegister ARMRegister ARMRegister
                          | QSUB16 ARMRegister ARMRegister ARMRegister
                          | QSUB8 ARMRegister ARMRegister ARMRegister
                          | QSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | SADD16 ARMRegister ARMRegister ARMRegister
                          | SADD8 ARMRegister ARMRegister ARMRegister
                          | SADDSUBX ARMRegister ARMRegister ARMRegister
                          | SSUB16 ARMRegister ARMRegister ARMRegister
                          | SSUB8 ARMRegister ARMRegister ARMRegister
                          | SSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | SHADD16 ARMRegister ARMRegister ARMRegister
                          | SHADD8 ARMRegister ARMRegister ARMRegister
                          | SHADDSUBX ARMRegister ARMRegister ARMRegister
                          | SHSUB16 ARMRegister ARMRegister ARMRegister
                          | SHSUB8 ARMRegister ARMRegister ARMRegister
                          | SHSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | UADD16 ARMRegister ARMRegister ARMRegister
                          | UADD8 ARMRegister ARMRegister ARMRegister
                          | UADDSUBX ARMRegister ARMRegister ARMRegister
                          | USUB16 ARMRegister ARMRegister ARMRegister
                          | USUB8 ARMRegister ARMRegister ARMRegister
                          | USUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | UHADD16 ARMRegister ARMRegister ARMRegister
                          | UHADD8 ARMRegister ARMRegister ARMRegister
                          | UHADDSUBX ARMRegister ARMRegister ARMRegister
                          | UHSUB16 ARMRegister ARMRegister ARMRegister
                          | UHSUB8 ARMRegister ARMRegister ARMRegister
                          | UHSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | UQADD16 ARMRegister ARMRegister ARMRegister
                          | UQADD8 ARMRegister ARMRegister ARMRegister
                          | UQADDSUBX ARMRegister ARMRegister ARMRegister
                          | UQSUB16 ARMRegister ARMRegister ARMRegister
                          | UQSUB8 ARMRegister ARMRegister ARMRegister
                          | UQSUBADDX ARMRegister ARMRegister ARMRegister
                          
                          | SXTAB16 ARMRegister ARMRegister ARMOpData -- rotate
                          | SXTAB ARMRegister ARMRegister ARMOpData -- rotate
                          | SXTAH ARMRegister ARMRegister ARMOpData -- rotate
                          | SXTB16 ARMRegister ARMOpData -- rotate
                          | SXT Width ARMRegister ARMOpData --rotate only -- ONLY SXTB, SXTH
                          | SXTB ARMRegister ARMOpData --rotate only -- NEW
                          | SXTH ARMRegister ARMOpData --rotate only -- NEW
                          | UXTAB16 ARMRegister ARMRegister ARMOpData -- rotate 
                          | UXTAB ARMRegister ARMRegister ARMOpData -- rotate
                          | UXTAH ARMRegister ARMRegister ARMOpData -- rotate
                          | UXTB16 ARMRegister ARMOpData -- rotate
                          | UXT Width ARMRegister ARMOpData -- rotate -- UXTB, UXTH
                          | UXTB Width ARMRegister ARMOpData -- rotate -- NEW
                          | UXTH Width ARMRegister ARMOpData -- rotate -- NEW

                          
                          | CLZ ARMRegister ARMRegister
                          | USAD8 ARMRegister ARMRegister ARMRegister 
                          | USADA8 ARMRegister ARMRegister ARMRegister ARMRegister
                          | PKHBT ARMRegister ARMRegister ARMOpData -- rotate/shift
                          | PKHTB ARMRegister ARMRegister ARMOpData -- rotate/shift
                          | REV ARMRegister ARMRegister
                          | REV16 ARMRegister ARMRegister
                          | REVSH ARMRegister ARMRegister
                          | SEL ARMRegister ARMRegister ARMRegister
                          | SSAT ARMRegister Word8 ARMOpData -- rotate/shift
                          | SSAT16 ARMRegister Word8 ARMRegister
                          | USAT ARMRegister Word8 ARMOpData -- rotate/shift
                          | USAT16 ARMRegister Word8 ARMRegister
                          
                          | MRS ARMRegister ARMStatusRegister
                          | MSR 
                          
                          | LDR Width Bool Bool ARMRegister ARMOpMemory -- LDR, LDRB, LDRH, LDRD, LDRT LDRBT, LDRSB, LDRSH -- TODO: some of these combinations are invalid, should we stop that?
                          | LDRB ARMRegister ARMOpMemory -- NEW
                          | LDRH ARMRegister ARMOpMemory -- NEW
                          | LDRD ARMRegister ARMOpMemory -- NEW
                          | LDRT ARMRegister ARMOpMemory -- NEW
                          | LDRBT ARMRegister ARMOpMemory -- NEW
                          | LDRSB ARMRegister ARMOpMemory -- NEW
                          | LDRSH ARMRegister ARMOpMemory -- NEW
                          | STR Width Bool Bool ARMRegister ARMOpMemory -- STR, STRB, STRH, STRD, STRT, STRBT
                          | STRB ARMRegister ARMOpMemory
                          | STRH ARMRegister ARMOpMemory
                          | STRD ARMRegister ARMOpMemory
                          | STRT ARMRegister ARMOpMemory
                          | STRBT ARMRegister ARMOpMemory
                          | LDREX
                          | STREX
                          
                          | LDM -- Note that there are three different forms
                          | STM -- Note that there are two different forms
                          | PUSH ARMOpMultiple
                          | POP ARMOpMultiple
                          
                          | SWP Bool ARMRegister ARMRegister ARMRegister -- SWP, SWPB
                          | SWPB Bool ARMRegister ARMRegister ARMRegister -- NEW

                          
                          | SWI Word32
                          
                          | DBG Word32
                          | DMB ARMHint
                          | DSB ARMHint
                          | ISB ARMHint
                          
                          | PLI ARMOpMemory
                          
                          | YIELD
                          | WFE
                          | WFI
                          | SEV
                          
                          | BFC ARMRegister (Maybe (Word32, Word32))
                          | BFI ARMRegister ARMRegister (Maybe (Word32, Word32)) -- come up with a nicer way to do this
                          | MLS ARMRegister ARMRegister ARMRegister
                          
                          | MOVW ARMRegister Word32
                          | MOVT ARMRegister Word32
                          | RBIT ARMRegister ARMRegister
  deriving (Show, Read, Eq)

data ARMUnconditionalOpcode = CPS
                            | SETEND ARMEndian
                            | RFE
                            | BKPT Word8
                            | PLD ARMOpMemory
                            | SRS
                            | BLXUC Int32 -- unconditional BLX
  deriving (Show, Read, Eq)