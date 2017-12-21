module Interpreter where

import MachineState
import Boilerplate
import InsSet
import Control.Monad.State.Lazy

exec :: Ins -> State MBRegisters ()
exec (Add ra rb rd) = do
  a <- readRegisterSt ra
  b <- readRegisterSt rb
  writeRegisterSt rd (a + b)
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"
