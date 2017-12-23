module TestData where

import           Boilerplate
import           Control.Monad.State.Lazy
import           InsSet
import           Interpreter
import           MachineState

quickTest :: State MicroBlaze W32
quickTest = do
  setRegister R1 (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S)
  setRegister R2 (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S)
  exec (Add R1 R2 R3)
  x <- getRegister R3
  return x
