{-|
Module : TestData
Description : example data and basic tests
-}
{-# LANGUAGE UnicodeSyntax #-}
module TestData where

import           Boilerplate
import qualified Boilerplate.W32          as W32
import qualified Boilerplate.W8           as W8
import           Control.Monad.State.Lazy
import           InsSet
import           Interpreter
import           MachineState

-- -- | An addition test
-- quickTest ∷ State MicroBlaze W32
-- quickTest = do
--   setRegister R1 (W32 W8.zero W8.zero W8.zero (W8 C C C C C C C S))
--   setRegister R2 (W32 W8.zero W8.zero W8.zero (W8 C C C C C C C S))
--   exec (Add R1 R2 R3)
--   x ← getRegister R3
--   return x


