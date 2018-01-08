{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where

import           Boilerplate
import qualified Boilerplate.Bit          as B
import qualified Boilerplate.W32          as W32
import           Control.Monad.State.Lazy
import           InsSet
import           MachineState


type Op = (W32 → W32 → W32)

execTypeA ∷ Op → MBReg → MBReg → MBReg → State MicroBlaze ()
execTypeA op rd ra rb = do
  a ← getRegister ra
  b ← getRegister rb
  setRegister rd $ op a b

execTypeB ∷ Op → MBReg → MBReg → W16 → State MicroBlaze ()
execTypeB op dest ra imm = do
  a ← getRegister ra
  let b = W32.signExtendW16 imm
  setRegister dest $ op a b



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
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


type CarryFlag = Bool
type KeepFlag = Bool
type DelayFlag = Bool

data BranchInput = TypeA MBReg MBReg
                 | TypeB MBReg W16

data AbsoluteBranchInput = AbsR MBReg
                         | AbsI W16

link ∷ MBReg → State MicroBlaze ()
link rd = do
  pc ← getRPC
  setRegister rd pc

delay ∷ State MicroBlaze ()
delay = setMSRBit DelayEnable S

getBranchInputValue ∷ BranchInput → State MicroBlaze W32
getBranchInputValue (TypeA _ rb)  = getRegister rb
getBranchInputValue (TypeB _ w16) = return $ W32.signExtendW16 w16

getBranchRegisterA ∷ BranchInput → State MicroBlaze W32
getBranchRegisterA (TypeA ra _) = getRegister ra
getBranchRegisterA (TypeB ra _) = getRegister ra

absoluteBranch ∷ AbsoluteBranchInput → State MicroBlaze ()
absoluteBranch (AbsR rb) = do
  b ← getRegister rb
  setRPC b
absoluteBranch (AbsI imm) = setRPC $ W32.signExtendW16 imm

branch ∷ BranchInput  → (MBWord → Bit) → State MicroBlaze ()
branch input branch_test = do
  a ← getBranchRegisterA input
  case branch_test a of
    C -> return ()
    S -> do
      b ← getBranchInputValue input
      pc ← getRPC
      setRPC $ snd $ W32.add b pc C

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

data LoadSize = LWord
              | LHalfWord (W16 → W32)
              | LByte (W8 → W32)

data ImmOrReg  = Register  MBReg
               | Immediate W16

load :: LoadSize → MBReg → MBReg → ImmOrReg  → State MicroBlaze ()
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



