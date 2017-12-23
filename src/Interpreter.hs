module Interpreter where

import           Boilerplate
import           Control.Monad.State.Lazy
import           InsSet
import           MachineState

{-
exec :: Ins -> State MBRegisters ()
exec (Add ra rb rd) = do
  a <- readRegisterSt ra
  b <- readRegisterSt rb
  writeRegisterSt rd (a + b)
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"
-}
