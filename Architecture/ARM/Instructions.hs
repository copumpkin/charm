module Architecture.ARM.Instructions where

import Prelude hiding (and)

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
                    | ARMUndefined
  deriving (Show, Read, Eq)

data ARMConditionalOpcode = B Bool Int32 -- B, BL
                          | BL Int32 -- NEW
                          | BLX ARMRegister -- conditional form
                          | BX ARMRegister
                          | BXJ ARMRegister
                          
                          | AND  ARMRegister ARMRegister ARMOpData -- AND, ANDS
                          | ANDS ARMRegister ARMRegister ARMOpData -- NEW
                          
                          | EOR  ARMRegister ARMRegister ARMOpData -- EOR, EORS
                          | EORS ARMRegister ARMRegister ARMOpData -- NEW
                          
                          | SUB  ARMRegister ARMRegister ARMOpData -- SUB, SUBS
                          | SUBS ARMRegister ARMRegister ARMOpData -- NEW

                          | RSB  ARMRegister ARMRegister ARMOpData -- RSB, RSBS
                          | RSBS ARMRegister ARMRegister ARMOpData -- NEW

                          | ADD  ARMRegister ARMRegister ARMOpData -- ADD, ADDS
                          | ADDS ARMRegister ARMRegister ARMOpData -- NEW

                          | ADC  ARMRegister ARMRegister ARMOpData -- ADC, ADCS
                          | ADCS ARMRegister ARMRegister ARMOpData -- NEW

                          | SBC  ARMRegister ARMRegister ARMOpData -- SBC, SBCS
                          | SBCS ARMRegister ARMRegister ARMOpData -- NEW

                          | RSC  ARMRegister ARMRegister ARMOpData -- RSC, RSCS
                          | RSCS ARMRegister ARMRegister ARMOpData -- NEW

                          | TST ARMRegister ARMOpData -- TST
                          | TEQ ARMRegister ARMOpData -- TEQ
                          | CMP ARMRegister ARMOpData -- CMP
                          | CMN ARMRegister ARMOpData -- CMN

                          | ORR  ARMRegister ARMRegister ARMOpData
                          | ORRS ARMRegister ARMRegister ARMOpData

                          | MOV  ARMRegister ARMOpData
                          | MOVS ARMRegister ARMOpData

                          | LSL  ARMRegister ARMOpData
                          | LSLS ARMRegister ARMOpData

                          | LSR  ARMRegister ARMOpData
                          | LSRS ARMRegister ARMOpData

                          | ASR  ARMRegister ARMOpData
                          | ASRS ARMRegister ARMOpData

                          | RRX  ARMRegister ARMRegister 
                          | RRXS ARMRegister ARMRegister

                          | ROR  ARMRegister ARMOpData
                          | RORS ARMRegister ARMOpData

                          | BIC  ARMRegister ARMRegister ARMOpData
                          | BICS ARMRegister ARMRegister ARMOpData

                          | MVN  ARMRegister ARMOpData
                          | MVNS ARMRegister ARMOpData
                                                    
                          | MLA  ARMRegister ARMRegister ARMRegister ARMRegister
                          | MLAS ARMRegister ARMRegister ARMRegister ARMRegister

                          | MUL  ARMRegister ARMRegister ARMRegister 
                          | MULS ARMRegister ARMRegister ARMRegister 

 
                          | SMLA Nybble Nybble ARMRegister ARMRegister ARMRegister ARMRegister -- SMLABB, SMLABT, SBMLATB, SMLATT
                          | SMLABB ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLABT ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLATB ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
                          | SMLATT ARMRegister ARMRegister ARMRegister ARMRegister -- NEW
 
                          | SMLAD  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLADX ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMLAL  ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME: are first two ArmRegisters more complicated?
                          | SMLALS ARMRegister ARMRegister ARMRegister ARMRegister -- FIXME: are first two ArmRegisters more complicated?

                          | SMLALBB ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLALBT ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLALTB ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLALTT ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMLALD  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLALDX ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMLAW Nybble ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMLSD  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLSDX ARMRegister ARMRegister ARMRegister ARMRegister
                          
                          | SMLSLD  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMLSLDX ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMMLA  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMMLAR ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMMUL  ARMRegister ARMRegister ARMRegister 
                          | SMMULR ARMRegister ARMRegister ARMRegister 

                          | SMMLS  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMMLSR ARMRegister ARMRegister ARMRegister ARMRegister

                          | SMUAD  ARMRegister ARMRegister ARMRegister
                          | SMUADX ARMRegister ARMRegister ARMRegister
 
                          | SMUL Nybble Nybble ARMRegister ARMRegister ARMRegister
                          | SMULBB ARMRegister ARMRegister ARMRegister -- NEW
                          | SMULBT ARMRegister ARMRegister ARMRegister -- NEW
                          | SMULTB ARMRegister ARMRegister ARMRegister -- NEW
                          | SMULTT ARMRegister ARMRegister ARMRegister -- NEW
 
                          | SMULL  ARMRegister ARMRegister ARMRegister ARMRegister
                          | SMULLS ARMRegister ARMRegister ARMRegister ARMRegister                          
                          
                          | SMULW Nybble ARMRegister ARMRegister ARMRegister
                          
                          | SMULWB ARMRegister ARMRegister ARMRegister -- NEW
                          | SMULWT ARMRegister ARMRegister ARMRegister -- NEW
                          
                          | SMUSD  ARMRegister ARMRegister ARMRegister
                          | SMUSDX ARMRegister ARMRegister ARMRegister

                          | UMAAL ARMRegister ARMRegister ARMRegister ARMRegister
                     
                          | UMLAL  ARMRegister ARMRegister ARMRegister ARMRegister
                          | UMLALS ARMRegister ARMRegister ARMRegister ARMRegister

                          | UMULL  ARMRegister ARMRegister ARMRegister ARMRegister
                          | UMULLS ARMRegister ARMRegister ARMRegister ARMRegister
                                                    
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
                          | UXTB ARMRegister ARMOpData -- rotate -- NEW
                          | UXTH ARMRegister ARMOpData -- rotate -- NEW

                          
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
                          
                          | LDR  ARMRegister ARMOpMemory 
                          | LDRB ARMRegister ARMOpMemory 
                          | LDRH ARMRegister ARMOpMemory 
                          | LDRD ARMRegister ARMOpMemory 

                          | LDRBT ARMRegister ARMOpMemory
                          | LDRHT ARMRegister ARMOpMemory
                          | LDRT  ARMRegister ARMOpMemory

                          | LDRSB ARMRegister ARMOpMemory
                          | LDRSH ARMRegister ARMOpMemory 
                          
                          
                          | STR  ARMRegister ARMOpMemory -- STR, STRB, STRH, STRD, STRT, STRBT
                          | STRB ARMRegister ARMOpMemory
                          | STRH ARMRegister ARMOpMemory
                          | STRD ARMRegister ARMOpMemory

                          | STRBT ARMRegister ARMOpMemory
                          | STRHT ARMRegister ARMOpMemory
                          | STRT ARMRegister ARMOpMemory



                          | LDREX ARMRegister ARMRegister ARMOpMemory
                          | STREX ARMRegister ARMRegister ARMOpMemory
                          | STREXD ARMRegister ARMRegister ARMRegister ARMOpMemory                          
                          
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

cond :: a -> a -> Bool -> a
cond f t False = f
cond f t True  = t

swp :: Bool -> ARMRegister -> ARMRegister -> ARMRegister -> ARMConditionalOpcode
swp False = undefined
swp True = undefined

and = cond AND ANDS
eor = cond EOR EORS
sub = cond SUB SUBS
rsb = cond RSB RSBS
add = cond ADD ADDS
adc = cond ADC ADCS
sbc = cond SBC SBCS
rsc = cond RSC RSCS

smuad  = cond SMUAD SMUADX
smusd  = cond SMUSD SMUSDX
smlad  = cond SMLAD SMLADX
smlald = cond SMLALD SMLALDX
smlsd  = cond SMLSD SMLSDX
smlsld = cond SMLSLD SMLSLDX

orr = cond ORR ORRS
mov = cond MOV MOVS
lsl = cond LSL LSLS
lsr = cond LSR LSRS
asr = cond ASR ASRS
rrx = cond RRX RRXS
ror = cond ROR RORS
bic = cond BIC BICS
mvn = cond MVN MVNS


smmla = cond SMMLA SMMLAR
smmul = cond SMMUL SMMULR
smmls = cond SMMLS SMMLSR

mul = cond MUL MULS
mla = cond MLA MLAS

umlal = cond UMLAL UMLALS
umull = cond UMULL UMULLS

smlal = cond SMLAL SMLALS
smull = cond SMULL SMULLS

ldr :: Width -> Bool -> Bool -> ARMRegister -> ARMOpMemory -> ARMConditionalOpcode
ldr Byte       False  False = LDRB 
ldr Byte       False  True  = LDRSB 
ldr Byte       True   False = LDRBT
ldr HalfWord   False  False = LDRH
ldr HalfWord   False  True  = LDRH
ldr HalfWord   True   False = LDRHT
ldr Word       False  False = LDR
ldr Word       True   False = LDRT
ldr DoubleWord False  False = LDRD
ldr _ _ _ = error "invalid combination of LDR flags"

str :: Width -> Bool -> ARMRegister -> ARMOpMemory -> ARMConditionalOpcode
str Byte       False  = STRB 
str Byte       True   = STRBT
str HalfWord   False  = STRH
str HalfWord   True   = STRHT
str Word       False  = STR
str Word       True   = STRT
str DoubleWord False  = STRD
str _ _ = error "invalid combination of STR flags"