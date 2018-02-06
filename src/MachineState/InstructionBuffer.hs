-- |
{-# LANGUAGE UnicodeSyntax #-}
module MachineState.InstructionBuffer where

import           Boilerplate
import qualified Boilerplate.W32 as W32
import           InsSet


data InstructionBuffer = InstructionBuffer { _decode  ∷ Maybe W32
                                           , _execute ∷ Maybe Ins }


type Address = W32

emptyInstructionBuffer :: InstructionBuffer
emptyInstructionBuffer = InstructionBuffer  Nothing Nothing

