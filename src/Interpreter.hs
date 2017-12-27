{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where

import           Boilerplate
import           Control.Monad.State.Lazy
import           InsSet
import           MachineState


exec :: Ins -> State MicroBlaze ()
exec (Add rd ra rb)     = addition False False ra rb rd
exec (Addc rd ra rb)    = addition True False ra rb rd
exec (Addk rd ra rb)    = addition False True ra rb rd
exec (Addkc rd ra rb)   = addition True True ra rb rd
exec (Addi rd ra imm)   = immediateAddition False False ra imm rd
exec (Addic rd ra imm)  = immediateAddition True False ra imm rd
exec (Addik rd ra imm)  = immediateAddition False True ra imm rd
exec (Addikc rd ra imm) = immediateAddition True True ra imm rd
exec (And rd ra rb)     = do
  a ← getRegister ra
  b ← getRegister rb
  setRegister rd $ andW32 a b
exec (Andi rd ra imm)   = do
  a ← getRegister ra
  let b = signExtendW16 imm
  setRegister rd $ andW32 a b
exec (Andn rd ra rb)    = do
  a ← getRegister ra
  b ← getRegister rb
  setRegister rd $ andW32 a (notW32 b)
exec (Andni rd ra imm)  = do
  a ← getRegister ra
  let b = signExtendW16 imm
  setRegister rd $ andW32 a (notW32 b)
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


type CarryFlag = Bool
type KeepFlag = Bool


addition ∷ CarryFlag → KeepFlag → MBReg → MBReg → MBReg → State MicroBlaze ()
addition carry_flag keep_flag ra rb rd = do
  a ← getRegister ra
  b ← getRegister rb
  c ← if carry_flag then getMSRBit Carry else return C
  let (carry, output) = plusCW32 a b c
  setRegister rd output
  if keep_flag
    then return ()
    else setMSRBit Carry carry

immediateAddition ∷ CarryFlag → KeepFlag → MBReg → W16 → MBReg → State MicroBlaze ()
immediateAddition carry_flag keep_flag ra imm rd = do
  a ← getRegister ra
  let b = signExtendW16 imm
  c ← if carry_flag then getMSRBit Carry else return C
  let (carry, output) = plusCW32 a b c
  setRegister rd output
  if keep_flag
    then return ()
    else setMSRBit Carry carry

