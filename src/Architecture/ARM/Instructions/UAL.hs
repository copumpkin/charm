module Architecture.ARM.Instructions.UAL where

import Prelude hiding (and)

import Architecture.ARM.Common

import Data.Int
import Data.Word

{-
data ARMOpRegister = Reg ARMRegister
                   | RegBang ARMRegister
  deriving (Show, Read, Eq)
-}

data ARMOpData = Imm Int32
               | Reg ARMRegister
               | RegShiftImm ARMShift Int32 ARMRegister 
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

data UALInstruction = Unconditional Unconditional
                    | Conditional Condition Conditional
                    | Undefined
  deriving (Show, Read, Eq)

-- Todo: use a GADT here and index constructors by whether they're conditional or not?
data Conditional = B Int32
                 | BL Int32
                 | BLX ARMRegister -- conditional form
                 | BX ARMRegister
                 | BXJ ARMRegister
                 
                 | AND  ARMRegister ARMRegister ARMOpData
                 | ANDS ARMRegister ARMRegister ARMOpData
                                                         
                 | EOR  ARMRegister ARMRegister ARMOpData
                 | EORS ARMRegister ARMRegister ARMOpData
                                                         
                 | SUB  ARMRegister ARMRegister ARMOpData
                 | SUBS ARMRegister ARMRegister ARMOpData
                                                         
                 | RSB  ARMRegister ARMRegister ARMOpData
                 | RSBS ARMRegister ARMRegister ARMOpData
                                                         
                 | ADD  ARMRegister ARMRegister ARMOpData
                 | ADDS ARMRegister ARMRegister ARMOpData
                                                         
                 | ADC  ARMRegister ARMRegister ARMOpData
                 | ADCS ARMRegister ARMRegister ARMOpData
                                                         
                 | SBC  ARMRegister ARMRegister ARMOpData
                 | SBCS ARMRegister ARMRegister ARMOpData
                                                         
                 | RSC  ARMRegister ARMRegister ARMOpData
                 | RSCS ARMRegister ARMRegister ARMOpData
                                                         
                 | TST ARMRegister ARMOpData
                 | TEQ ARMRegister ARMOpData
                 | CMP ARMRegister ARMOpData
                 | CMN ARMRegister ARMOpData
                 
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
                 
                 
                 | SMLABB ARMRegister ARMRegister ARMRegister ARMRegister 
                 | SMLABT ARMRegister ARMRegister ARMRegister ARMRegister 
                 | SMLATB ARMRegister ARMRegister ARMRegister ARMRegister 
                 | SMLATT ARMRegister ARMRegister ARMRegister ARMRegister 
                                                                          
                 | SMLAWB ARMRegister ARMRegister ARMRegister ARMRegister 
                 | SMLAWT ARMRegister ARMRegister ARMRegister ARMRegister 
                 
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
                 
                 | SMULBB ARMRegister ARMRegister ARMRegister
                 | SMULBT ARMRegister ARMRegister ARMRegister
                 | SMULTB ARMRegister ARMRegister ARMRegister
                 | SMULTT ARMRegister ARMRegister ARMRegister
                 
                 | SMULL  ARMRegister ARMRegister ARMRegister ARMRegister
                 | SMULLS ARMRegister ARMRegister ARMRegister ARMRegister                          
                                  
                 | SMULWB ARMRegister ARMRegister ARMRegister
                 | SMULWT ARMRegister ARMRegister ARMRegister
                 
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
                 | QASX ARMRegister ARMRegister ARMRegister
                 | QDADD ARMRegister ARMRegister ARMRegister
                 | QDSUB ARMRegister ARMRegister ARMRegister
                 | QSUB ARMRegister ARMRegister ARMRegister
                 | QSUB16 ARMRegister ARMRegister ARMRegister
                 | QSUB8 ARMRegister ARMRegister ARMRegister
                 | QSAX ARMRegister ARMRegister ARMRegister
                 
                 | SADD16 ARMRegister ARMRegister ARMRegister
                 | SADD8 ARMRegister ARMRegister ARMRegister
                 | SASX ARMRegister ARMRegister ARMRegister
                 | SSUB16 ARMRegister ARMRegister ARMRegister
                 | SSUB8 ARMRegister ARMRegister ARMRegister
                 | SSAX ARMRegister ARMRegister ARMRegister
                 
                 | SHADD16 ARMRegister ARMRegister ARMRegister
                 | SHADD8 ARMRegister ARMRegister ARMRegister
                 | SHASX ARMRegister ARMRegister ARMRegister
                 | SHSUB16 ARMRegister ARMRegister ARMRegister
                 | SHSUB8 ARMRegister ARMRegister ARMRegister
                 | SHSAX ARMRegister ARMRegister ARMRegister
                 
                 | UADD16 ARMRegister ARMRegister ARMRegister
                 | UADD8 ARMRegister ARMRegister ARMRegister
                 | UASX ARMRegister ARMRegister ARMRegister
                 | USUB16 ARMRegister ARMRegister ARMRegister
                 | USUB8 ARMRegister ARMRegister ARMRegister
                 | USAX ARMRegister ARMRegister ARMRegister
                 
                 | UHADD16 ARMRegister ARMRegister ARMRegister
                 | UHADD8 ARMRegister ARMRegister ARMRegister
                 | UHASX ARMRegister ARMRegister ARMRegister
                 | UHSUB16 ARMRegister ARMRegister ARMRegister
                 | UHSUB8 ARMRegister ARMRegister ARMRegister
                 | UHSAX ARMRegister ARMRegister ARMRegister
                 
                 | UQADD16 ARMRegister ARMRegister ARMRegister
                 | UQADD8 ARMRegister ARMRegister ARMRegister
                 | UQASX ARMRegister ARMRegister ARMRegister
                 | UQSUB16 ARMRegister ARMRegister ARMRegister
                 | UQSUB8 ARMRegister ARMRegister ARMRegister
                 | UQSAX ARMRegister ARMRegister ARMRegister
                 
                 | SXTAB16 ARMRegister ARMRegister ARMOpData -- rotate
                 | SXTAB ARMRegister ARMRegister ARMOpData -- rotate
                 | SXTAH ARMRegister ARMRegister ARMOpData -- rotate
                 | SXTB16 ARMRegister ARMOpData -- rotate
                 | SXTB ARMRegister ARMOpData --rotate only -- NEW
                 | SXTH ARMRegister ARMOpData --rotate only -- NEW
                 | UXTAB16 ARMRegister ARMRegister ARMOpData -- rotate 
                 | UXTAB ARMRegister ARMRegister ARMOpData -- rotate
                 | UXTAH ARMRegister ARMRegister ARMOpData -- rotate
                 | UXTB16 ARMRegister ARMOpData -- rotate
                 | UXTB ARMRegister ARMOpData -- rotate -- NEW
                 | UXTH ARMRegister ARMOpData -- rotate -- NEW
                 
                 | UBFX ARMRegister ARMRegister Word32 Word32
                 | SBFX ARMRegister ARMRegister Word32 Word32
                 
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
                 | MSR Bool Bool ARMOpData -- FIXME: always immediate, we should fix this. make the o parser's internals reusable
                 
                 | LDR  ARMRegister ARMOpMemory 
                 | LDRB ARMRegister ARMOpMemory 
                 | LDRH ARMRegister ARMOpMemory 
                 | LDRD ARMRegister ARMOpMemory 
                 
                 | LDRBT ARMRegister ARMOpMemory
                 | LDRHT ARMRegister ARMOpMemory
                 | LDRT  ARMRegister ARMOpMemory
                 
                 | LDRSB  ARMRegister ARMOpMemory
                 | LDRSBT ARMRegister ARMOpMemory
                 
                 | LDRSH  ARMRegister ARMOpMemory 
                 | LDRSHT ARMRegister ARMOpMemory 
                 
                 | STR  ARMRegister ARMOpMemory
                 | STRB ARMRegister ARMOpMemory
                 | STRH ARMRegister ARMOpMemory
                 | STRD ARMRegister ARMOpMemory
                 
                 | STRBT ARMRegister ARMOpMemory
                 | STRHT ARMRegister ARMOpMemory
                 | STRT ARMRegister ARMOpMemory
                 
                 
                 
                 | LDREX  ARMRegister ARMOpMemory
                 | LDREXB ARMRegister ARMOpMemory
                 | LDREXH ARMRegister ARMOpMemory
                 | LDREXD ARMRegister ARMRegister ARMOpMemory
                 
                 | STREX  ARMRegister ARMRegister ARMOpMemory
                 | STREXB ARMRegister ARMRegister ARMOpMemory
                 | STREXH ARMRegister ARMRegister ARMOpMemory
                 | STREXD ARMRegister ARMRegister ARMRegister ARMOpMemory
                                           
                 | LDM   Bool ARMRegister ARMOpMultiple -- == LDMIA /LDMFD
                 | LDMDA Bool ARMRegister ARMOpMultiple -- == LDMFA
                 | LDMDB Bool ARMRegister ARMOpMultiple -- == LDMEA
                 | LDMIB Bool ARMRegister ARMOpMultiple -- == LDMED
                 
                 | STM   Bool ARMRegister ARMOpMultiple -- == STMIA / STMFD
                 | STMDA Bool ARMRegister ARMOpMultiple -- == STMFA
                 | STMDB Bool ARMRegister ARMOpMultiple -- == STMEA
                 | STMIB Bool ARMRegister ARMOpMultiple -- == STMED
                 
                 | PUSH ARMOpMultiple
                 | POP ARMOpMultiple
                 
                 | SWP ARMRegister ARMRegister ARMOpMemory -- Last reg is actually a memory offset
                 | SWPB ARMRegister ARMRegister ARMOpMemory -- Last reg is actually a memory offset
                 
                 | SMC Word32
                 | SVC Word32
                 
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
                 | MLS ARMRegister ARMRegister ARMRegister ARMRegister
                 
                 | MOVW ARMRegister Word32
                 | MOVT ARMRegister Word32
                 | RBIT ARMRegister ARMRegister
                 
                 | NOP 
  deriving (Show, Read, Eq)

data Unconditional = CPS Word32
                   | CPSIE Bool Bool Bool (Maybe Word32)
                   | CPSID Bool Bool Bool (Maybe Word32)
                   | SETEND ARMEndian
                   
                   | RFE   Bool ARMRegister
                   | RFEDA Bool ARMRegister
                   | RFEDB Bool ARMRegister
                   | RFEIB Bool ARMRegister
                   
                   | BKPT Word8
                   | PLD ARMOpMemory
                   
                   | SRS   Bool ARMRegister Word32 -- the register is always SP/R13
                   | SRSDA Bool ARMRegister Word32 -- the register is always SP/R13
                   | SRSDB Bool ARMRegister Word32 -- the register is always SP/R13
                   | SRSIB Bool ARMRegister Word32 -- the register is always SP/R13
                   
                   | CLREX
                   | BLXUC Int32 -- unconditional BLX
  deriving (Show, Read, Eq)

cond :: a -> a -> Bool -> a
cond f t False = f
cond f t True  = t



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

ldr :: Width -> Bool -> Bool -> ARMRegister -> ARMOpMemory -> Conditional
ldr Byte       False  False = LDRB 
ldr Byte       False  True  = LDRSB 
ldr Byte       True   False = LDRBT
ldr Byte       True   True  = LDRSBT
ldr Halfword   False  False = LDRH
ldr Halfword   False  True  = LDRSH
ldr Halfword   True   False = LDRHT
ldr Halfword   True   True  = LDRSHT
ldr Word       False  False = LDR
ldr Word       True   False = LDRT
ldr Doubleword False  False = LDRD
ldr _ _ _ = error "invalid combination of LDR flags"

str :: Width -> Bool -> ARMRegister -> ARMOpMemory -> Conditional
str Byte       False  = STRB 
str Byte       True   = STRBT
str Halfword   False  = STRH
str Halfword   True   = STRHT
str Word       False  = STR
str Word       True   = STRT
str Doubleword False  = STRD
str _ _ = error "invalid combination of STR flags"

ldm Decrement After  = LDMDA
ldm Decrement Before = LDMDB
ldm Increment After  = LDM
ldm Increment Before = LDMIB

stm Decrement After  = STMDA
stm Decrement Before = STMDB
stm Increment After  = STM
stm Increment Before = STMIB

rfe Decrement After  = RFEDA
rfe Decrement Before = RFEDB
rfe Increment After  = RFE
rfe Increment Before = RFEIB

srs Decrement After  = SRSDA
srs Decrement Before = SRSDB
srs Increment After  = SRS
srs Increment Before = SRSIB

swp = cond SWP SWPB


b = cond B BL

isBranch :: UALInstruction -> Bool
isBranch (Conditional _ (B _)) = True
isBranch (Conditional _ (BL _)) = True
isBranch (Conditional _ (BLX _)) = True
isBranch (Conditional _ (BX _)) = True
isBranch (Conditional _ (BXJ _)) = True
-- A bunch more
isBranch _ = False