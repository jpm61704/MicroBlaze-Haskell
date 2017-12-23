module Interpreter where

import           Boilerplate
import           Control.Monad.State.Lazy
import           InsSet
import           MachineState


exec :: Ins -> State MicroBlaze ()
exec (Add ra rb rd) = do
  a <- getRegister ra
  b <- getRegister rb
  setRegister rd $ plusW32 a b C
exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"

