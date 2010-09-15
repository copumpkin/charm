{-# LANGUAGE EmptyDataDecls, TypeFamilies, GADTs, StandaloneDeriving, FlexibleInstances #-}
module Architecture.ARM.Instructions.UAL where

import Prelude hiding (and)

import Architecture.ARM.Common

import Data.Int
import Data.Word hiding (Word)

data UAL = UAL

data DataOp = Imm Int32
            | Reg Register
            | RegShiftImm Shift Int32 Register  -- FIXME: why is this an Int32?
            | RegShiftReg Shift Register Register
            | RegShiftRRX Register
  deriving (Show, Read, Eq)
  
data MemOp = MemReg Register DataOp Bool
           | MemRegNeg Register DataOp Bool
           | MemRegPost Register DataOp
           | MemRegPostNeg Register DataOp
  deriving (Show, Read, Eq)

data MultiRegOp = Regs [Register]
                | RegsCaret [Register]
  deriving (Show, Read, Eq)


instance InstructionSet UAL where
  data Instruction UAL c where
    B   :: Int32     -> Instruction UAL Conditional
    BL  :: Int32     -> Instruction UAL Conditional
    BX  :: Register  -> Instruction UAL Conditional
    BLX :: DataOp    -> Instruction UAL a           -- Supports both conditional and unconditional forms! ArmOpData can only be Reg or Imm. Maybe one day we'll enforce that, but it'll make things ugly.
    BXJ :: Register  -> Instruction UAL Conditional
    
    AND  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ANDS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    EOR  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    EORS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SUB  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SUBS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    RSB  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    RSBS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ADD  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ADDS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ADC  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ADCS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SBC  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SBCS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    RSC  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    RSCS :: Register -> Register -> DataOp -> Instruction UAL Conditional
                 
    TST  :: Register -> DataOp -> Instruction UAL Conditional
    TEQ  :: Register -> DataOp -> Instruction UAL Conditional
    CMP  :: Register -> DataOp -> Instruction UAL Conditional
    CMN  :: Register -> DataOp -> Instruction UAL Conditional

    ORR  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    ORRS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    
    MOV  :: Register -> DataOp -> Instruction UAL Conditional
    MOVS :: Register -> DataOp -> Instruction UAL Conditional
    LSL  :: Register -> DataOp -> Instruction UAL Conditional
    LSLS :: Register -> DataOp -> Instruction UAL Conditional
    LSR  :: Register -> DataOp -> Instruction UAL Conditional
    LSRS :: Register -> DataOp -> Instruction UAL Conditional
    ASR  :: Register -> DataOp -> Instruction UAL Conditional
    ASRS :: Register -> DataOp -> Instruction UAL Conditional
    
    RRX  :: Register -> Register -> Instruction UAL Conditional 
    RRXS :: Register -> Register -> Instruction UAL Conditional
    
    ROR  :: Register -> DataOp -> Instruction UAL Conditional
    RORS :: Register -> DataOp -> Instruction UAL Conditional
    
    BIC  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    BICS :: Register -> Register -> DataOp -> Instruction UAL Conditional
    
    MVN  :: Register -> DataOp -> Instruction UAL Conditional
    MVNS :: Register -> DataOp -> Instruction UAL Conditional
    
    MLA  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    MLAS :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    MUL  :: Register -> Register -> Register -> Instruction UAL Conditional 
    MULS :: Register -> Register -> Register -> Instruction UAL Conditional 
    
    SMLABB  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLABT  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLATB  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLATT  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
                                                            
    SMLAWB  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLAWT  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    SMLAD   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLADX  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    SMLAL   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional-- FIXME: are first two Registers more complicated?
    SMLALS  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional-- FIXME: are first two Registers more complicated?
    
    SMLALBB :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLALBT :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLALTB :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLALTT :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    SMLALD  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLALDX :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    SMLSD   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLSDX  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    SMLSLD  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMLSLDX :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    SMMLA   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMMLAR  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    SMMUL   :: Register -> Register -> Register -> Instruction UAL Conditional 
    SMMULR  :: Register -> Register -> Register -> Instruction UAL Conditional 
            
    SMMLS   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMMLSR  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    SMUAD   :: Register -> Register -> Register -> Instruction UAL Conditional
    SMUADX  :: Register -> Register -> Register -> Instruction UAL Conditional
            
    SMULBB  :: Register -> Register -> Register -> Instruction UAL Conditional
    SMULBT  :: Register -> Register -> Register -> Instruction UAL Conditional
    SMULTB  :: Register -> Register -> Register -> Instruction UAL Conditional
    SMULTT  :: Register -> Register -> Register -> Instruction UAL Conditional
            
    SMULL   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    SMULLS  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional                         
                    
    SMULWB  :: Register -> Register -> Register -> Instruction UAL Conditional
    SMULWT  :: Register -> Register -> Register -> Instruction UAL Conditional
            
    SMUSD   :: Register -> Register -> Register -> Instruction UAL Conditional
    SMUSDX  :: Register -> Register -> Register -> Instruction UAL Conditional
            
    UMAAL   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    UMLAL   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    UMLALS  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
            
    UMULL   :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    UMULLS  :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    QADD    :: Register -> Register -> Register -> Instruction UAL Conditional
    QADD16  :: Register -> Register -> Register -> Instruction UAL Conditional
    QADD8   :: Register -> Register -> Register -> Instruction UAL Conditional
    QASX    :: Register -> Register -> Register -> Instruction UAL Conditional
    QDADD   :: Register -> Register -> Register -> Instruction UAL Conditional
    QDSUB   :: Register -> Register -> Register -> Instruction UAL Conditional
    QSUB    :: Register -> Register -> Register -> Instruction UAL Conditional
    QSUB16  :: Register -> Register -> Register -> Instruction UAL Conditional
    QSUB8   :: Register -> Register -> Register -> Instruction UAL Conditional
    QSAX    :: Register -> Register -> Register -> Instruction UAL Conditional

    SADD16  :: Register -> Register -> Register -> Instruction UAL Conditional
    SADD8   :: Register -> Register -> Register -> Instruction UAL Conditional
    SASX    :: Register -> Register -> Register -> Instruction UAL Conditional
    SSUB16  :: Register -> Register -> Register -> Instruction UAL Conditional
    SSUB8   :: Register -> Register -> Register -> Instruction UAL Conditional
    SSAX    :: Register -> Register -> Register -> Instruction UAL Conditional

    SHADD16 :: Register -> Register -> Register -> Instruction UAL Conditional
    SHADD8  :: Register -> Register -> Register -> Instruction UAL Conditional
    SHASX   :: Register -> Register -> Register -> Instruction UAL Conditional
    SHSUB16 :: Register -> Register -> Register -> Instruction UAL Conditional
    SHSUB8  :: Register -> Register -> Register -> Instruction UAL Conditional
    SHSAX   :: Register -> Register -> Register -> Instruction UAL Conditional

    UADD16  :: Register -> Register -> Register -> Instruction UAL Conditional
    UADD8   :: Register -> Register -> Register -> Instruction UAL Conditional
    UASX    :: Register -> Register -> Register -> Instruction UAL Conditional
    USUB16  :: Register -> Register -> Register -> Instruction UAL Conditional
    USUB8   :: Register -> Register -> Register -> Instruction UAL Conditional
    USAX    :: Register -> Register -> Register -> Instruction UAL Conditional

    UHADD16 :: Register -> Register -> Register -> Instruction UAL Conditional
    UHADD8  :: Register -> Register -> Register -> Instruction UAL Conditional
    UHASX   :: Register -> Register -> Register -> Instruction UAL Conditional
    UHSUB16 :: Register -> Register -> Register -> Instruction UAL Conditional
    UHSUB8  :: Register -> Register -> Register -> Instruction UAL Conditional
    UHSAX   :: Register -> Register -> Register -> Instruction UAL Conditional

    UQADD16 :: Register -> Register -> Register -> Instruction UAL Conditional
    UQADD8  :: Register -> Register -> Register -> Instruction UAL Conditional
    UQASX   :: Register -> Register -> Register -> Instruction UAL Conditional
    UQSUB16 :: Register -> Register -> Register -> Instruction UAL Conditional
    UQSUB8  :: Register -> Register -> Register -> Instruction UAL Conditional
    UQSAX   :: Register -> Register -> Register -> Instruction UAL Conditional

    SXTAB16 :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SXTAB   :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SXTAH   :: Register -> Register -> DataOp -> Instruction UAL Conditional
    SXTB16  :: Register -> DataOp -> Instruction UAL Conditional
    SXTB    :: Register -> DataOp -> Instruction UAL Conditional
    SXTH    :: Register -> DataOp -> Instruction UAL Conditional
    UXTAB16 :: Register -> Register -> DataOp -> Instruction UAL Conditional 
    UXTAB   :: Register -> Register -> DataOp -> Instruction UAL Conditional
    UXTAH   :: Register -> Register -> DataOp -> Instruction UAL Conditional
    UXTB16  :: Register -> DataOp -> Instruction UAL Conditional
    UXTB    :: Register -> DataOp -> Instruction UAL Conditional
    UXTH    :: Register -> DataOp -> Instruction UAL Conditional

                 
    UBFX :: Register -> Register -> Word32 -> Word32 -> Instruction UAL Conditional
    SBFX :: Register -> Register -> Word32 -> Word32 -> Instruction UAL Conditional
    
    CLZ    :: Register -> Register -> Instruction UAL Conditional

    USAD8  :: Register -> Register -> Register -> Instruction UAL Conditional 
    USADA8 :: Register -> Register -> Register -> Register -> Instruction UAL Conditional

    PKHBT  :: Register -> Register -> DataOp -> Instruction UAL Conditional
    PKHTB  :: Register -> Register -> DataOp -> Instruction UAL Conditional

    REV    :: Register -> Register -> Instruction UAL Conditional
    REV16  :: Register -> Register -> Instruction UAL Conditional
    REVSH  :: Register -> Register -> Instruction UAL Conditional

    SEL    :: Register -> Register -> Register -> Instruction UAL Conditional

    SSAT   :: Register -> Word8 -> DataOp -> Instruction UAL Conditional
    SSAT16 :: Register -> Word8 -> Register -> Instruction UAL Conditional
    USAT   :: Register -> Word8 -> DataOp -> Instruction UAL Conditional
    USAT16 :: Register -> Word8 -> Register -> Instruction UAL Conditional

    MRS :: Register -> StatusRegister -> Instruction UAL Conditional
    MSR :: Bool -> Bool -> DataOp -> Instruction UAL Conditional -- FIXME: always immediate, we should fix this. make the o parser's internals reusable

    LDR    :: Register -> MemOp -> Instruction UAL Conditional
    LDRB   :: Register -> MemOp -> Instruction UAL Conditional
    LDRH   :: Register -> MemOp -> Instruction UAL Conditional
    LDRD   :: Register -> MemOp -> Instruction UAL Conditional
    LDRBT  :: Register -> MemOp -> Instruction UAL Conditional
    LDRHT  :: Register -> MemOp -> Instruction UAL Conditional
    LDRT   :: Register -> MemOp -> Instruction UAL Conditional
    LDRSB  :: Register -> MemOp -> Instruction UAL Conditional
    LDRSBT :: Register -> MemOp -> Instruction UAL Conditional
    LDRSH  :: Register -> MemOp -> Instruction UAL Conditional
    LDRSHT :: Register -> MemOp -> Instruction UAL Conditional
    
    STR    :: Register -> MemOp -> Instruction UAL Conditional
    STRB   :: Register -> MemOp -> Instruction UAL Conditional
    STRH   :: Register -> MemOp -> Instruction UAL Conditional
    STRD   :: Register -> MemOp -> Instruction UAL Conditional
    STRBT  :: Register -> MemOp -> Instruction UAL Conditional
    STRHT  :: Register -> MemOp -> Instruction UAL Conditional
    STRT   :: Register -> MemOp -> Instruction UAL Conditional
    
    
    
    LDREX  :: Register -> MemOp -> Instruction UAL Conditional
    LDREXB :: Register -> MemOp -> Instruction UAL Conditional
    LDREXH :: Register -> MemOp -> Instruction UAL Conditional
    LDREXD :: Register -> Register -> MemOp -> Instruction UAL Conditional

    STREX  :: Register -> Register -> MemOp -> Instruction UAL Conditional
    STREXB :: Register -> Register -> MemOp -> Instruction UAL Conditional
    STREXH :: Register -> Register -> MemOp -> Instruction UAL Conditional
    STREXD :: Register -> Register -> Register -> MemOp -> Instruction UAL Conditional 
                            
    LDM    :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == LDMIA /LDMFD
    LDMDA  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == LDMFA
    LDMDB  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == LDMEA
    LDMIB  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == LDMED
                                                
    STM    :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == STMIA / STMFD
    STMDA  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == STMFA
    STMDB  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == STMEA
    STMIB  :: Bool -> Register -> MultiRegOp -> Instruction UAL Conditional -- == STMED

    PUSH :: MultiRegOp -> Instruction UAL Conditional
    POP  :: MultiRegOp -> Instruction UAL Conditional

    SWP  :: Register -> Register -> MemOp -> Instruction UAL Conditional -- Last reg is actually a memory offset
    SWPB :: Register -> Register -> MemOp -> Instruction UAL Conditional -- Last reg is actually a memory offset

    SMC :: Word32 -> Instruction UAL Conditional
    SVC :: Word32 -> Instruction UAL Conditional

    DBG :: Word32 -> Instruction UAL Conditional
    DMB :: Hint -> Instruction UAL Conditional
    DSB :: Hint -> Instruction UAL Conditional
    ISB :: Hint -> Instruction UAL Conditional
    
    PLI :: MemOp -> Instruction UAL Conditional
    
    YIELD :: Instruction UAL Conditional
    WFE :: Instruction UAL Conditional
    WFI :: Instruction UAL Conditional
    SEV :: Instruction UAL Conditional
    
    BFC :: Register -> Maybe (Word32, Word32) -> Instruction UAL Conditional
    BFI :: Register -> Register -> Maybe (Word32, Word32) -> Instruction UAL Conditional -- come up with a nicer way to do this
    MLS :: Register -> Register -> Register -> Register -> Instruction UAL Conditional
    
    MOVW :: Register -> Word32 -> Instruction UAL Conditional
    MOVT :: Register -> Word32 -> Instruction UAL Conditional
    RBIT :: Register -> Register -> Instruction UAL Conditional
    
    NOP :: Instruction UAL Conditional

    CPS    :: Word32 -> Instruction UAL Unconditional
    CPSIE  :: Bool -> Bool -> Bool -> Maybe Word32 -> Instruction UAL Unconditional
    CPSID  :: Bool -> Bool -> Bool -> Maybe Word32 -> Instruction UAL Unconditional
    SETEND :: Endianness -> Instruction UAL Unconditional

    RFE    :: Bool -> Register -> Instruction UAL Unconditional
    RFEDA  :: Bool -> Register -> Instruction UAL Unconditional
    RFEDB  :: Bool -> Register -> Instruction UAL Unconditional
    RFEIB  :: Bool -> Register -> Instruction UAL Unconditional

    BKPT   :: Word32 -> Instruction UAL Unconditional
    PLD    :: MemOp  -> Instruction UAL Unconditional

    SRS    :: Bool -> Register -> Word32 -> Instruction UAL Unconditional -- the register is always SP/R13
    SRSDA  :: Bool -> Register -> Word32 -> Instruction UAL Unconditional -- the register is always SP/R13
    SRSDB  :: Bool -> Register -> Word32 -> Instruction UAL Unconditional -- the register is always SP/R13
    SRSIB  :: Bool -> Register -> Word32 -> Instruction UAL Unconditional -- the register is always SP/R13
    
    CLREX  :: Instruction UAL Unconditional

    CBNZ   :: Register -> Word32 -> Instruction UAL Unconditional
    CBZ    :: Register -> Word32 -> Instruction UAL Unconditional

    IT     :: ITSpecifier -> Instruction UAL Unconditional

deriving instance Show (Instruction UAL c)

-- Handy psuedo-accessors to use the flexible blx
blxc :: DataOp -> Instruction UAL Conditional
blxc = BLX

blxu :: DataOp -> Instruction UAL Unconditional
blxu = BLX

cond :: a -> a -> Bool -> a
cond f t False = f
cond f t True  = t


-- Convenience pseudo-constructors. I should probably just factor out the common behavior into the constructors, really.

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

ldr :: Width -> Bool -> Bool -> Register -> MemOp -> Instruction UAL Conditional
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

str :: Width -> Bool -> Register -> MemOp -> Instruction UAL Conditional
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
