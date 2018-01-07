{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where

import           Boilerplate
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
exec (Beq ra rb _)      = branch False (ra, Left rb)   ((W32.==) W32.zero)
exec (Beqd ra rb _)     = branch True  (ra, Left rb)   ((W32.==) W32.zero)
exec (Beqi ra imm)      = branch False (ra, Right imm) ((W32.==) W32.zero)
exec (Beqid ra imm)     = branch True  (ra, Right imm) ((W32.==) W32.zero)
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


type CarryFlag = Bool
type KeepFlag = Bool
type DelayFlag = Bool


branch ∷ DelayFlag → (MBReg, (Either MBReg W16)) → (MBWord → Bit) → State MicroBlaze ()
branch delay (ra, y) branch_test = do
  a ← getRegister ra
  case branch_test a of
    C -> return ()
    S -> do
      if delay
        then setMSRBit DelayEnable S
        else return ()
      b ← case y of
            Left rb   → getRegister rb
            Right imm → return $ W32.signExtendW16 imm
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





