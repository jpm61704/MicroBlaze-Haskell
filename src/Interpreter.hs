{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where

import           Boilerplate
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
  let b = signExtendW16 imm
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
exec (And rd ra rb)     = execTypeA andW32 rd ra rb
exec (Andi rd ra imm)   = execTypeB andW32 rd ra imm
exec (Andn rd ra rb)    = execTypeA (\a b→ andW32 a (notW32 b)) rd ra rb
exec (Andni rd ra imm)  = execTypeB (\a b→ andW32 a (notW32 b)) rd ra imm
exec (Beq ra rb _)      = branch False (ra, Left rb)   ((==) zero32)
exec (Beqd ra rb _)     = branch True  (ra, Left rb)   ((==) zero32)
exec (Beqi ra imm)      = branch False (ra, Right imm) ((==) zero32)
exec (Beqid ra imm)     = branch True  (ra, Right imm) ((==) zero32)
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


type CarryFlag = Bool
type KeepFlag = Bool
type DelayFlag = Bool


branch ∷ DelayFlag → (MBReg, (Either MBReg W16)) → (MBWord → Bool) → State MicroBlaze ()
branch delay (ra, y) branch_test = do
  a ← getRegister ra
  if branch_test a
    then do
      if delay
        then setMSRBit DelayEnable S
        else return ()
      b ← case y of
            Left rb   → getRegister rb
            Right imm → return $ signExtendW16 imm
      pc ← getRPC
      setRPC $ plusW32 b pc C
    else return ()


add :: CarryFlag → KeepFlag → (MBReg, Either MBReg W16) → MBReg → State MicroBlaze ()
add carry keep (ra, y) rd = do
  a ← getRegister ra
  b ← case y of
        Left rb   → getRegister rb
        Right imm → return $ signExtendW16 imm
  c ← if carry then getMSRBit Carry else return C
  let (carry_out, output) = plusCW32 a b c
  setRegister rd output
  if keep
    then return ()
    else setMSRBit Carry carry_out





