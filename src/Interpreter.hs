{-|
Module : Interpreter
Description: The instruction interpreter for the MicroBlaze processor
-}
{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where

import           Boilerplate
import qualified Boilerplate.Bit                    as B
import qualified Boilerplate.W32                    as W32
import           Control.Monad.State.Lazy
import           InsSet
import           MachineState
import           MachineState.MachineStatusRegister


-- * Exec Function
-- Processes instructions within the MachineState Data Type
-- Control flow is handled elsewhere

-- | note that all exec does is process the given instructions as it can given
-- the machine state information. Delays and other temporal actions must be handled
-- at a different level of the program
exec :: Ins -> State MicroBlaze ()
exec (Add rd ra rb)     = add False False (ra, Left rb) rd
exec (Addc rd ra rb)    = add True False (ra, Left rb) rd
exec (Addk rd ra rb)    = add False True (ra, Left rb) rd
exec (Addkc rd ra rb)   = add True True (ra, Left rb) rd
exec (Addi rd ra imm)   = add False False (ra, Right imm) rd
exec (Addic rd ra imm)  = add True False (ra, Right imm) rd
exec (Addik rd ra imm)  = add False True (ra, Right imm) rd
exec (Addikc rd ra imm) = add True True (ra, Right imm) rd
exec (And rd ra rb)     = execTypeA W32.and rd ra rb
exec (Andi rd ra imm)   = execTypeB W32.and rd ra imm
exec (Andn rd ra rb)    = execTypeA (\a b→ W32.and a (W32.not b)) rd ra rb
exec (Andni rd ra imm)  = execTypeB (\a b→ W32.and a (W32.not b)) rd ra imm
exec (Beq ra rb _)      = branch (TypeA ra rb) ((W32.==) W32.zero)
exec (Beqd ra rb _)     = delay >> branch (TypeA ra rb ) ((W32.==) W32.zero)
exec (Beqi ra imm)      = branch (TypeB ra imm) ((W32.==) W32.zero)
exec (Beqid ra imm)     = delay >> branch (TypeB ra imm) ((W32.==) W32.zero)
exec (Bge ra rb _)      = branch (TypeA ra rb) W32.isPositive
exec (Bged ra rb _)     = delay >> branch (TypeA ra rb) W32.isPositive
exec (Bgei ra imm)      = branch (TypeB ra imm) W32.isPositive
exec (Bgeid ra imm)     = delay >> branch (TypeB ra imm) W32.isPositive
exec (Bgt ra rb _)      = branch (TypeA ra rb) W32.greaterThanZero
exec (Bgtd ra rb _)     = delay >> branch (TypeA ra rb) W32.greaterThanZero
exec (Bgti ra imm)      = branch (TypeB ra imm) W32.greaterThanZero
exec (Bgtid ra imm)     = delay >> branch (TypeB ra imm) W32.greaterThanZero
exec (Ble ra rb _)      = branch (TypeA ra rb) W32.lessThanOrEqualToZero
exec (Bled ra rb _)     = delay >> branch (TypeA ra rb) W32.lessThanOrEqualToZero
exec (Blei ra imm)      = branch (TypeB ra imm) W32.lessThanOrEqualToZero
exec (Bleid ra imm)     = delay >> branch (TypeB ra imm) W32.lessThanOrEqualToZero
exec (Blt ra rb _)      = branch (TypeA ra rb) W32.isNegative
exec (Bltd ra rb _)     = delay >> branch (TypeA ra rb) W32.isNegative
exec (Blti ra imm)      = branch (TypeB ra imm) W32.isNegative
exec (Bltid ra imm)     = delay >> branch (TypeB ra imm) W32.isNegative
exec (Bne ra rb _)      = branch (TypeA ra rb) (\x → B.not (x W32.== W32.zero))
exec (Bned ra rb _)     = delay >> branch (TypeA ra rb) (\x → B.not (x W32.== W32.zero))
exec (Bnei ra imm)      = branch (TypeB ra imm) (\x → B.not (x W32.== W32.zero))
exec (Bneid ra imm)     = delay >> branch (TypeB ra imm) (\x → B.not (x W32.== W32.zero))
exec (Br rb)            = branch (TypeA undefined rb) (\x → S)
exec (Bra rb)           = absoluteBranch (AbsR rb)
exec (Brd rb)           = delay >> branch (TypeA undefined rb) (\x → S)
exec (Brad rb)          = delay >> absoluteBranch (AbsR rb)
exec (Brld rd rb)       = (link rd) >> delay >> branch (TypeA undefined rb) (\x → S)
exec (Brald rd rb)      = (link rd) >> delay >> absoluteBranch (AbsR rb)
exec (Bri imm)          = branch (TypeB undefined imm) (\x → S)
exec (Brai imm)         = absoluteBranch (AbsI imm)
exec (Brid imm)         = delay >> branch (TypeB undefined imm) (\x → S)
exec (Braid imm)        = delay >> absoluteBranch (AbsI imm)
exec (Brlid rd imm)     = (link rd) >> delay >> branch (TypeB undefined imm) (\x → S)
exec (Bralid rd imm)    = (link rd) >> delay >> absoluteBranch (AbsI imm)
exec (Brk rd rb)        = link rd >> getRegister rb >>= setRPC >> setMSRBit BreakInProgress S
exec (Brki rd imm)      = link rd >> setRPC (W32.signExtendW16 imm) >> setMSRBit BreakInProgress S
exec (Bsrl rd ra rb)    = error $ "barrel shifts not yet implemented"
exec (Bsra rd ra rb)    = error $ "barrel shifts not yet implemented"
exec (Bsll rd ra rb)    = error $ "barrel shifts not yet implemented"
exec (Bsrli rd ra imm)  = error $ "barrel shifts not yet implemented"
exec (Bsrai rd ra imm)  = error $ "barrel shifts not yet implemented"
exec (Bslli rd ra imm)  = error $ "barrel shifts not yet implemented"
exec (Cmp rd ra rb)     = execTypeA W32.signedCompare rd ra rb
exec (Cmpu rd ra rb)    = execTypeA W32.unsignedCompare rd ra rb
exec (Get rd fslx)      = error $ "fsl instructions not yet implemented"
exec (Nget rd fslx)     = error $ "fsl instructions not yet implemented"
exec (Cget rd fslx)     = error $ "fsl instructions not yet implemented"
exec (Ncget rd fslx)    = error $ "fsl instructions not yet implemented"
exec (Idiv rd ra rb)    = error $ "requires hardware divider implementation"
exec (Idivu rd ra rb)   = error $ "requires hardware divider implementation"
exec (Imm imm)          = setRegister R18 (W32.backExtendW16 imm) -- NOTE: I may want to set a flag here
exec (Lbu rd ra rb)     = load (LByte W32.unsignedExtendW8) rd ra (Register rb)
exec (Lbui rd ra imm)   = load (LByte W32.unsignedExtendW8) rd ra (Immediate imm)
exec (Lhu rd ra rb)     = load (LHalfWord W32.unsignedExtendW16) rd ra (Register rb)
exec (Lhui rd ra imm)   = load (LHalfWord W32.unsignedExtendW16) rd ra (Immediate imm)
exec (Lw rd ra rb)      = load LWord rd ra (Register rb)
exec (Lwi rd ra imm)    = load LWord rd ra (Immediate imm)
exec (Mfs rd rs)        = moveFromSRegister rd rs
exec (Mts rs ra )       = moveToSRegister rs ra
exec (Mul rd ra rb)     = undefined
exec (Muli rd ra imm)   = undefined
exec (Or rd ra rb)      = execTypeA W32.or rd ra rb
exec (Ori rd ra imm)    = execTypeB W32.or rd ra imm
exec (Put ra fslx)      = error $ "fsl instructions not yet implemented"
exec (Nput ra fslx)     = error $ "fsl instructions not yet implemented"
exec (Cput ra fslx)     = error $ "fsl instructions not yet implemented"
exec (Ncput ra fslx)    = error $ "fsl instructions not yet implemented"
exec (Rsub rd ra rb)    = sub False False (ra, Left rb) rd
exec (Rsubc rd ra rb)   = sub True False (ra, Left rb) rd
exec (Rsubk rd ra rb)   = sub False True (ra, Left rb) rd
exec (Rsubkc rd ra rb)  = sub True True (ra, Left rb) rd
exec (Rsubi rd ra imm)  = sub False False (ra, Right imm) rd
exec (Rsubic rd ra imm) = sub True False (ra, Right imm) rd
exec (Rsubik rd ra imm) = sub False True (ra, Right imm) rd
exec (Rsubikc rd ra imm) = sub True True (ra, Right imm) rd
exec (Rtbd ra imm)      = returnFrom ra imm >> setMSRBit BreakInProgress C
exec (Rtid ra imm)      = returnFrom ra imm >> setMSRBit InterruptEnable S
exec (Rtsd ra imm)      = returnFrom ra imm
exec (Sb rd ra rb)      = store SByte rd ra (Register rb)
exec (Sbi rd ra imm)    = store SByte rd ra (Immediate imm)
exec (Sext8 rd ra)      = sext8 rd ra
exec (Sext16 rd ra)     = sext16 rd ra
exec (Sh rd ra rb)      = store SHalfWord rd ra (Register rb)
exec (Shi rd ra imm)    = store SHalfWord rd ra (Immediate imm)
exec (Sra rd ra)        = shiftRightArithmetic False rd ra
exec (Src rd ra)        = shiftRightArithmetic True rd ra
exec (Srl rd ra)        = shiftRightLogical rd ra
exec (Sw rd ra rb)      = store SWord rd ra (Register rb)
exec (Swi rd ra imm)    = store SWord rd ra (Immediate imm)
exec (Wdc _ _)          = error "cache instructions not available"
exec (Wic _ _)          = error "cache instructions not available"
exec (Xor rd ra rb)     = execTypeA W32.xor rd ra rb
exec (Xori rd ra imm)   = execTypeB W32.xor rd ra imm
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


-- * Utility Functions and Data Types


-- | Delay Flag for Branching
type DelayFlag = Bool


-- | Branch Input Type
data BranchInput = TypeA MBReg MBReg
                 | TypeB MBReg W16

-- | Absolute Branch Input Type
data AbsoluteBranchInput = AbsR MBReg
                         | AbsI W16

-- | W32 Isomorphism type
type Op = (W32 → W32 → W32)

-- ** Generic Operator Execution

-- | execute a type a instruction using basic operators
execTypeA ∷ Op                   -- ^ Operator to apply to values
          → MBReg                -- ^ Destination register
          → MBReg                -- ^ Input Register A
          → MBReg                -- ^ Input Register B
          → State MicroBlaze ()
execTypeA op rd ra rb = do
  a ← getRegister ra
  b ← getRegister rb
  setRegister rd $ op a b

-- | execute a type b instruction using basic operators
execTypeB ∷ Op                   -- ^ Operator to apply to values
          → MBReg                -- ^ Destination register
          → MBReg                -- ^ Input Register A
          → W16                  -- ^ Immediate Data
          → State MicroBlaze ()
execTypeB op dest ra imm = do
  a ← getRegister ra
  let b = W32.signExtendW16 imm
  setRegister dest $ op a b


-- ** Branching

-- | SHOULD BE DEPRECATED
getBranchInputValue ∷ BranchInput → State MicroBlaze W32
getBranchInputValue (TypeA _ rb)  = getRegister rb
getBranchInputValue (TypeB _ w16) = return $ W32.signExtendW16 w16

-- | SHOULD BE DEPRECATED
getBranchRegisterA ∷ BranchInput → State MicroBlaze W32
getBranchRegisterA (TypeA ra _) = getRegister ra
getBranchRegisterA (TypeB ra _) = getRegister ra

-- | branch to an absolute address
absoluteBranch ∷ AbsoluteBranchInput → State MicroBlaze ()
absoluteBranch (AbsR rb) = do
  b ← getRegister rb
  setRPC b
absoluteBranch (AbsI imm) = setRPC $ W32.signExtendW16 imm

-- | branch to a relative address
branch ∷ BranchInput          -- ^ The branch input type (TypeA vs TypeB)
       → (MBWord → Bit)      -- ^ The predicate to decide branching
       → State MicroBlaze ()
branch input branch_test = do
  a ← getBranchRegisterA input
  case branch_test a of
    C -> return ()
    S -> do
      b ← getBranchInputValue input
      pc ← getRPC
      setRPC $ snd $ W32.add b pc C

-- ** Addition

-- | adding mechanism for MicroBlaze (likely can be deprecated)
add :: CarryFlag → KeepFlag → (MBReg, Either MBReg W16) → MBReg → State MicroBlaze ()
add carry keep (ra, y) rd = do
  a ← getRegister ra
  b ← case y of
        Left rb   → getRegister rb
        Right imm → return $ W32.signExtendW16 imm
  c ← if carry then getMSRBit Carry else return C
  let (carry_out, output) = W32.add a b c
  setRegister rd output
  if keep
    then return ()
    else setMSRBit Carry carry_out

-- | hardware subtraction (likely can be deprecated)
sub :: CarryFlag → KeepFlag → (MBReg, Either MBReg W16) → MBReg → State MicroBlaze ()
sub carry keep (ra, y) rd = do
  a ← getRegister ra
  b ← case y of
        Left rb   → getRegister rb
        Right imm → return $ W32.signExtendW16 imm
  c ← if carry then getMSRBit Carry else return C
  let (carry_out, output) = W32.reverseSubtraction a b c
  setRegister rd output
  if keep
    then return ()
    else setMSRBit Carry carry_out

-- | Carry Flag for adder
type CarryFlag = Bool

-- | Keep Flag for adder
type KeepFlag = Bool


-- ** Loading From Memory

-- | size of load operation, include sign-extension function of appropriate size
data LoadSize = LWord
              | LHalfWord (W16 → W32)
              | LByte (W8 → W32)

-- | Either-like datatype to differentiate TypeA and TypeB data
data ImmOrReg  = Register  MBReg
               | Immediate W16

-- | Loads data from memory, The two register offsets are added to obtain an address
load :: LoadSize                 -- ^ the size of the load operation (Byte, HalfWord, Word)
     → MBReg                    -- ^ Destination register for loaded data
     → MBReg                    -- ^ Register Offset 1
     → ImmOrReg                 -- ^ Register Offset 2
     → State MicroBlaze ()
load s rd ra y = do
  a ← getRegister ra
  b ← case y of
        Register rb   → getRegister rb
        Immediate imm → return (W32.signExtendW16 imm)
  let address = snd $ W32.add a b C
  val ← case s of
              LWord            → loadWord address
              LHalfWord extend → do
                x ← loadHalfWord address
                return $ extend x
              LByte extend     → do
                x ← loadByte address
                return $ extend x
  setRegister rd val

data StoreSize = SWord | SHalfWord | SByte

store ∷ StoreSize
      → MBReg
      → MBReg
      → ImmOrReg
      → State MicroBlaze ()
store s rd ra y = do
  a ← getRegister ra
  b ← case y of
        Register rb   → getRegister rb
        Immediate imm → return (W32.signExtendW16 imm)
  d ← getRegister rd
  case s of
    SWord     → storeWord d a b
    SHalfWord → storeHalfWord (W32.leastSignificantHalfWord d) a b
    SByte     → storeByte (W32.leastSignificantByte d) a b


-- ** Special Purpose Registers

-- | pulls either the MSR or PC register into given register
moveFromSRegister ∷ MBReg                   -- ^ Destination Register
                  → MBSReg                  -- ^ Special Purpose Register to Pull (MSR or RPC)
                  → State MicroBlaze ()
moveFromSRegister rd rs = do
  sreg ← case rs of
           MSR → pullMSR
           RPC → getRPC
  setRegister rd sreg

-- | puts a Word into a special purpose register.
-- Does not support updates to the program counter
moveToSRegister ∷ MBSReg                -- ^ The register to alter
                → MBReg                 -- ^ The register containing the MSR Word
                → State MicroBlaze ()
moveToSRegister RPC _  = error "Illegal op: Cannot set program counter using MTS)"
moveToSRegister MSR ra = getRegister ra >>= pushMSR


-- | links the current program counter value into the specified register
link ∷ MBReg                     -- ^ The destination register for the PC word
     → State MicroBlaze ()
link rd = do
  pc ← getRPC
  setRegister rd pc


-- | sets the delay flag in the machine status register
delay ∷ State MicroBlaze ()
delay = setMSRBit DelayEnable S


-- | returns the pc from a break, interrupt, or subroutine
returnFrom ∷ MBReg → W16 → State MicroBlaze ()
returnFrom ra imm = do
  a ← getRegister ra
  let b = W32.signExtendW16 imm
  setRPC $ snd (W32.add a b C)

sext8 ∷ MBReg → MBReg → State MicroBlaze ()
sext8 rd ra = do
  a ← getRegister ra
  setRegister rd $ W32.signExtendW8 $ W32.leastSignificantByte a

sext16 ∷ MBReg → MBReg → State MicroBlaze ()
sext16 rd ra = do
  a ← getRegister ra
  setRegister rd $ W32.signExtendW16 $ W32.leastSignificantHalfWord a

shiftRightArithmetic ∷ CarryFlag → MBReg → MBReg → State MicroBlaze ()
shiftRightArithmetic carry_flag rd ra = do
  a ← getRegister ra
  c ← if carry_flag then getMSRBit Carry else return C
  let (carry, d) = W32.arithmeticShiftRight a c
  setMSRBit Carry carry
  setRegister rd d

shiftRightLogical ∷ MBReg → MBReg → State MicroBlaze ()
shiftRightLogical rd ra = do
  a ← getRegister ra
  let (carry, d) = W32.logicalShiftRight a
  setMSRBit Carry carry
  setRegister rd d
